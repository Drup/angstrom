(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type input_descr =
  { length      : int
  ; get         : int -> char
  ; substring   : int -> int -> string }

let input_descr_of_string s =
  { length    = String.length s
  ; get       = (fun pos     -> String.unsafe_get s pos)
  ; substring = (fun pos len -> String.sub s pos len) }

let input_descr_of_bigstring b =
  { length    = Bigarray.Array1.dim b
  ; get       = (fun pos     -> Bigarray.Array1.unsafe_get b pos)
  ; substring = (fun pos len ->
      Cstruct.to_string (Cstruct.of_bigarray ~off:pos ~len b)) }

module State = struct
  type t =
    { mutable commit_pos : int
    ; initial_commit_pos : int
    ; length    : int
    ; get       : int -> char
    ; substring : int -> int -> string }

  let create initial_commit_pos (input:input_descr) =
    { commit_pos = initial_commit_pos
    ; initial_commit_pos
    ; length    = input.length
    ; get       = input.get
    ; substring = input.substring }

  let length { initial_commit_pos; length }  =
    length + initial_commit_pos

  let initial_commit_pos t =
    t.initial_commit_pos

  let commit_pos { commit_pos } =
    commit_pos

  let committed_bytes { commit_pos; initial_commit_pos } =
    commit_pos - initial_commit_pos

  let uncommitted_bytes { length; commit_pos }  =
    length - commit_pos

  let commit t pos =
    t.commit_pos <- pos

  let substring { substring; initial_commit_pos } pos len =
    let pos = pos - initial_commit_pos in
    substring pos len

  let get { get; initial_commit_pos } pos =
    let pos = pos - initial_commit_pos in
    get pos

  let count_while { length; get; initial_commit_pos } pos f =
    let i = ref (pos - initial_commit_pos) in
    while !i < length && f (get !i) do incr i; done;
    !i - (pos - initial_commit_pos)
end

type input =
  [ `String    of string
  | `Bigstring of bigstring ]

let input_length input =
  match input with
  | `String s    -> String.length s
  | `Bigstring b -> Bigarray.Array1.dim b

let input_descr_of_input input =
  match input with
  | `String s -> input_descr_of_string s
  | `Bigstring b -> input_descr_of_bigstring b

  (*
module Input = struct
  type t =
    { mutable commit_pos : int
    ; initial_commit_pos : int
    ; input : input
    }

  let create initial_commit_pos input =
    { commit_pos = initial_commit_pos
    ; initial_commit_pos
    ; input
    }

  let length { initial_commit_pos; input}  =
    input_length input + initial_commit_pos

  let committed_bytes { commit_pos; initial_commit_pos } =
    commit_pos - initial_commit_pos

  let initial_commit_pos t =
    t.initial_commit_pos

  let commit_pos t =
    t.commit_pos

  let uncommitted_bytes t =
    input_length t.input - commit_pos t

  let substring { initial_commit_pos; input } pos len =
    let off = pos - initial_commit_pos in
    match input with
    | `String    s -> String.sub s off len
    | `Bigstring b -> Cstruct.to_string (Cstruct.of_bigarray ~off ~len b)

  let get { initial_commit_pos; input } pos =
    let pos = pos - initial_commit_pos in
    match input with
    | `String s    -> String.unsafe_get s pos
    | `Bigstring b -> Bigarray.Array1.unsafe_get b pos


  let count_while { initial_commit_pos; input } pos f =
    let i = ref (pos - initial_commit_pos) in
    let len = input_length input in
    begin match input with
    | `String s    ->
      while !i < len && f (String.unsafe_get s !i) do incr i; done
    | `Bigstring b ->
      while !i < len && f (Bigarray.Array1.unsafe_get b !i) do incr i; done
    end;
    !i - (pos - initial_commit_pos)

  let commit t pos =
    t.commit_pos <- pos

end
*)

type _unconsumed =
  { buffer : bigstring
  ; off : int
  ; len : int }

(* Encapsulate state with an object, Smalltalk style. Callers are not putting
 * this in tight loops so none of that performance jibber jabber. *)
class buffer cstruct =
  let internal = ref cstruct in
  let _writable_space t =
    let { Cstruct.buffer; len } = !internal in
    Bigarray.Array1.dim buffer - len
  in
  let _trailing_space t =
    let { Cstruct.buffer; off; len } = !internal in
    Bigarray.Array1.dim buffer - (off + len)
  in
  let compress () =
    let off, len = 0, Cstruct.len !internal in
    let buffer = Cstruct.of_bigarray ~off ~len (!internal).Cstruct.buffer in
    Cstruct.blit !internal 0 buffer 0 len;
    internal := buffer
  in
  let grow to_copy =
    let init_size = Bigarray.Array1.dim (!internal).Cstruct.buffer in
    let size  = ref init_size in
    let space = _writable_space () in
    while space + !size - init_size < to_copy do
      size := (3 * !size) / 2
    done;
    let buffer = Cstruct.(set_len (create !size)) (!internal).Cstruct.len in
    Cstruct.blit !internal 0 buffer 0 (!internal).Cstruct.len;
    internal := buffer
  in
  let ensure_space len =
    (* XXX(seliopou): could use some heuristics here to determine whether its
     * worth it to compress or grow *)
    begin if _trailing_space () >= len then
      () (* there is enough room at the end *)
    else if _writable_space () >= len then
      compress ()
    else
      grow len
    end;
    (* The above will grow the internal buffer but not change the length of the
     * view into the buffer. So it's necesasry to add the desired length at
     * this point. *)
    internal := Cstruct.add_len !internal len
  in
object(self)
  method feed (input:input) =
    let len = input_length input in
    ensure_space len;
    let off = Cstruct.len !internal - len in
    match input with
    | `String s ->
      let allocator _ = Cstruct.sub !internal off len in
      ignore (Cstruct.of_string ~allocator s)
    | `Bigstring b ->
      Cstruct.blit (Cstruct.of_bigarray b) 0 !internal off len

  method commit len =
    internal := Cstruct.shift !internal len

  method internal =
    let { Cstruct.buffer; off; len } = !internal in
    Bigarray.Array1.sub buffer off len

  method uncommitted_with_shift n =
    let { Cstruct.buffer; off; len } = Cstruct.shift !internal n in
    { buffer; off; len }

  method uncommitted =
    self#uncommitted_with_shift 0
end

let buffer_of_cstruct cstruct =
  new buffer cstruct

let buffer_of_size size =
  new buffer Cstruct.(set_len (create size) 0)

let buffer_of_bigstring ?(off=0) ?len bigstring =
  buffer_of_cstruct (Cstruct.of_bigarray ~off ?len bigstring)

let buffer_of_unconsumed { buffer; off; len} =
  buffer_of_bigstring ~off ~len buffer

module Unbuffered = struct
  type more =
    | Complete
    | Incomplete

  type 'a state =
    | Partial of 'a partial
    | Done    of 'a * int
    | Fail    of string list * string
  and 'a partial =
    { committed : int
    ; continue  : input -> more -> 'a state }

  type 'a with_input =
    State.t ->  int -> more -> 'a

  type 'a failure = (string list -> string -> 'a state) with_input
  type ('a, 'r) success = ('a -> 'r state) with_input

  let fail_k    buf pos _ marks msg = Fail(marks, msg)
  let succeed_k buf pos _       v   = Done(v, pos - State.initial_commit_pos buf)

  type 'a t =
    { run : 'r. ('r failure -> ('a, 'r) success -> 'r state) with_input }

  let fail_to_string marks err =
    String.concat " > " marks ^ ": " ^ err

  let state_to_option = function
    | Done(v, _) -> Some v
    | _          -> None

  let state_to_result = function
    | Done(v, _)        -> Result.Ok v
    | Partial _         -> Result.Error "incomplete input"
    | Fail (marks, err) -> Result.Error (fail_to_string marks err)

  let parse ?(input=`String "") p =
    let input_descr = input_descr_of_input input in
    p.run (State.create 0 input_descr) 0 Incomplete fail_k succeed_k

  let parse_only p input =
    let input_descr = input_descr_of_input input in
    state_to_result (p.run (State.create 0 input_descr) 0 Complete fail_k succeed_k)
end

type more = Unbuffered.more =
  | Complete
  | Incomplete

type 'a state = 'a Unbuffered.state =
  | Partial of 'a partial
  | Done    of 'a * int
  | Fail    of string list * string
and 'a partial = 'a Unbuffered.partial =
  { committed : int
  ; continue  : input -> more -> 'a state }

type 'a t = 'a Unbuffered.t =
  { run : 'r. ('r Unbuffered.failure -> ('a, 'r) Unbuffered.success -> 'r state) Unbuffered.with_input }


module Buffered = struct
  type unconsumed = _unconsumed =
    { buffer : bigstring
    ; off : int
    ; len : int }

  type 'a state =
    | Partial of ([ input | `Eof ] -> 'a state)
    | Done    of 'a * unconsumed
    | Fail    of string list * string * unconsumed

  let from_unbuffered_state ~f buffer = function
    | Unbuffered.Partial p      -> Partial (f p)
    | Unbuffered.Done (v , con) -> Done(v, buffer#uncommitted_with_shift con)
    | Unbuffered.Fail (ms, err) -> Fail(ms, err, buffer#uncommitted)

  let parse ?(initial_buffer_size=0x1000) ?(input=`String "") p =
    if initial_buffer_size < 1 then
      failwith "parse: invalid argument, initial_buffer_size < 1";
    let initial_buffer_size = max initial_buffer_size (input_length input) in
    let buffer = buffer_of_size initial_buffer_size in
    buffer#feed input;
    let rec f p =
      ();
      function
      | `Eof  -> from_unbuffered_state buffer ~f (p.continue (`Bigstring buffer#internal) Complete)
      | #input as input ->
        buffer#commit p.committed;
        buffer#feed input;
        from_unbuffered_state buffer ~f (p.continue (`Bigstring buffer#internal) Incomplete)
    in
    from_unbuffered_state buffer ~f (Unbuffered.parse ~input:(`Bigstring buffer#internal) p)

  let feed state input =
    match state with
    | Partial k            -> k input
    | Fail(marks, msg, us) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffer = buffer_of_unconsumed us in
        buffer#feed input;
        Fail(marks, msg, buffer#uncommitted)
      end
    | Done(v, us) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffer = buffer_of_unconsumed us in
        buffer#feed input;
        Done(v, buffer#uncommitted)
      end

  let state_to_option = function
    | Done(v, _) -> Some v
    | _          -> None

  let state_to_result = function
    | Partial _           -> Result.Error "incomplete input"
    | Done(v, _)          -> Result.Ok v
    | Fail(marks, err, _) -> Result.Error (Unbuffered.fail_to_string marks err)

  let state_to_unconsumed = function
    | (Done(_, us) | Fail(_, _, us)) -> Some us
    | _                              -> None

end

let parse_only p input =
  Unbuffered.parse_only p input

let return =
  fun v ->
    { run = fun input pos more _fail succ ->
      succ input pos more v }

let fail msg =
  { run = fun input pos more fail succ ->
    fail input pos more [] msg
  }

let (>>=) p f =
  { run = fun input pos more fail succ ->
    let succ' input' pos' more' v = (f v).run input' pos' more' fail succ in
    p.run input pos more fail succ'
  }

let (>>|) p f =
  { run = fun input pos more fail succ ->
    let succ' input' pos' more' v = succ input' pos' more' (f v) in
    p.run input pos more fail succ'
  }

let (<$>) f m =
  m >>| f

let (<*>) f m =
  (* f >>= fun f -> m >>| f *)
  { run = fun input pos more fail succ ->
    let succ0 input0 pos0 more0 f =
      let succ1 input1 pos1 more1 m = succ input1 pos1 more1 (f m) in
      m.run input0 pos0 more0 fail succ1
    in
    f.run input pos more fail succ0 }

let lift f m =
  f <$> m

let lift2 f m1 m2 =
  { run = fun input pos more fail succ ->
    let succ1 input1 pos1 more1 m1 =
      let succ2 input2 pos2 more2 m2 = succ input2 pos2 more2 (f m1 m2) in
      m2.run input1 pos1 more1 fail succ2
    in
    m1.run input pos more fail succ1 }

let lift3 f m1 m2 m3 =
  { run = fun input pos more fail succ ->
    let succ1 input1 pos1 more1 m1 =
      let succ2 input2 pos2 more2 m2 =
        let succ3 input3 pos3 more3 m3 =
          succ input3 pos3 more3 (f m1 m2 m3) in
        m3.run input2 pos2 more2 fail succ3 in
      m2.run input1 pos1 more1 fail succ2
    in
    m1.run input pos more fail succ1 }

let lift4 f m1 m2 m3 m4 =
  { run = fun input pos more fail succ ->
    let succ1 input1 pos1 more1 m1 =
      let succ2 input2 pos2 more2 m2 =
        let succ3 input3 pos3 more3 m3 =
          let succ4 input4 pos4 more4 m4 =
            succ input4 pos4 more4 (f m1 m2 m3 m4) in
          m4.run input3 pos3 more3 fail succ4 in
        m3.run input2 pos2 more2 fail succ3 in
      m2.run input1 pos1 more1 fail succ2
    in
    m1.run input pos more fail succ1 }

let ( *>) a b =
  (* a >>= fun _ -> b *)
  { run = fun input pos more fail succ ->
    let succ' input' pos' more' _ = b.run input' pos' more' fail succ in
    a.run input pos more fail succ'
  }

let (<* ) a b =
  (* a >>= fun x -> b >>| fun _ -> x *)
  { run = fun input pos more fail succ ->
    let succ0 input0 pos0 more0 x =
      let succ1 input1 pos1 more1 _ = succ input1 pos1 more1 x in
      b.run input0 pos0 more0 fail succ1
    in
    a.run input pos more fail succ0 }

let (<?>) p mark =
  { run = fun input pos more fail succ ->
    let fail' input' pos' more' marks msg =
      fail input' pos' more' (mark::marks) msg in
    p.run input pos more fail' succ
  }

let (<|>) p q =
  { run = fun input pos more fail succ ->
    let fail' input' pos' more' marks msg =
      (* The only two constructors that introduce new failure continuations are
       * [<?>] and [<|>]. If the initial input position is less than the length
       * of the committed input, then calling the failure continuation will
       * have the effect of unwinding all choices and collecting marks along
       * the way. *)
      if pos < State.commit_pos input' then
        fail input' pos' more marks msg
      else
        q.run input' pos more' fail succ in
    p.run input pos more fail' succ
  }

(** BEGIN: getting input *)

let rec prompt state pos fail succ =
  let uncommitted_bytes = State.uncommitted_bytes state in
  let commit_pos        = State.commit_pos state in
  (* The continuation should not hold any references to input above. *)
  let continue input more =
    let input_descr = input_descr_of_input input in
    if input_descr.length < uncommitted_bytes then
      failwith "prompt: input shrunk!";
    let state = State.create commit_pos input_descr in
    if input_descr.length = uncommitted_bytes then
      if more = Complete then
        fail state pos Complete
      else
        prompt state pos fail succ
    else
      succ state pos more
  in
  Partial { committed = State.committed_bytes state; continue }

let demand_input =
  { run = fun input pos more fail succ ->
    match more with
    | Complete   -> fail input pos more [] "not enough input"
    | Incomplete ->
      let succ' input' pos' more' = succ input' pos' more' ()
      and fail' input' pos' more' = fail input' pos' more' [] "not enough input" in
      prompt input pos fail' succ'
  }

let want_input =
  { run = fun input pos more _fail succ ->
    if pos < State.length input then
      succ input pos more true
    else if more = Complete then
      succ input pos more false
    else
      let succ' input' pos' more' = succ input' pos' more' true
      and fail' input' pos' more' = succ input' pos' more' false in
      prompt input pos fail' succ'
  }

let ensure_suspended n input pos more fail succ =
  let rec go =
    { run = fun input' pos' more' fail' succ' ->
      if pos' + n <= State.length input' then
        succ' input' pos' more' ()
      else
        (demand_input *> go).run input' pos' more' fail' succ'
    }
  in
  (demand_input *> go).run input pos more fail succ

let unsafe_substring n =
  { run = fun input pos more fail succ ->
    succ input (pos + n) more (State.substring input pos n)
  }

let ensure n =
  { run = fun input pos more fail succ ->
    if pos + n <= State.length input then
      succ input pos more ()
    else
      ensure_suspended n input pos more fail succ
  }
  *> unsafe_substring n


(** END: getting input *)

let end_of_input =
  { run = fun input pos more fail succ ->
    if pos < State.length input then
      fail input pos more [] "end_of_input"
    else if more = Complete then
      succ input pos more ()
    else
      let succ' input' pos' more' = fail input' pos' more' [] "end_of_input"
      and fail' input' pos' more' = succ input' pos' more' () in
      prompt input pos fail' succ'
  }

let advance n =
  { run = fun input pos more _fail succ -> succ input (pos + n) more () }

let pos =
  { run = fun input pos more _fail succ -> succ input pos more pos }

let available =
  { run = fun input pos more _fail succ ->
    succ input pos more (State.length input - pos)
  }

let get_buffer_and_pos =
  { run = fun input pos more _fail succ -> succ input pos more (input, pos) }

let commit =
  { run = fun input pos more _fail succ ->
    State.commit input pos;
    succ input pos more () }

(* Do not use this if [p] contains a [commit]. *)
let unsafe_lookahead p =
  { run = fun input pos more fail succ ->
    let succ' input' _ more' v = succ input' pos more' v in
    p.run input pos more fail succ' }

let peek_char =
  { run = fun input pos more fail succ ->
    if pos < State.length input then
      succ input pos more (Some (State.get input pos))
    else if more = Complete then
      succ input pos more None
    else
      let succ' input' pos' more' =
        succ input' pos' more' (Some (State.get input' pos'))
      and fail' input' pos' more' =
        succ input' pos' more' None in
      prompt input pos fail' succ'
  }

let _char ~msg f =
  { run = fun input pos more fail succ ->
    if pos < State.length input then
      match f (State.get input pos) with
      | None   -> fail input pos more [] msg
      | Some v -> succ input (pos + 1) more v
    else
      let succ' input' pos' more' () =
        match f (State.get input' pos') with
        | None   -> fail input' pos' more' [] msg
        | Some v -> succ input' (pos' + 1) more' v
      in
      ensure_suspended 1 input pos more fail succ'
  }

let peek_char_fail =
  unsafe_lookahead (_char ~msg:"peek_char_fail" (fun c -> Some c))

let satisfy f =
  _char ~msg:"satisfy" (fun c -> if f c then Some c else None)

let skip f =
  _char ~msg:"skip" (fun c -> if f c then Some () else None)

let char c =
  satisfy (fun c' -> c = c') <?> (String.make 1 c)

let not_char c =
  satisfy (fun c' -> c <> c') <?> ("not " ^ String.make 1 c)

let any_char =
  _char ~msg:"any_char" (fun c -> Some c)

let count_while ?(init=0) f =
  (* NB: does not advance position. *)
  let rec go acc =
    { run = fun input pos more fail succ ->
      let n = State.count_while input (pos + acc) f in
      let acc' = n + acc in
      (* Check if the loop terminated because it reached the end of the input
       * buffer. If so, then prompt for additional input and continue. *)
      if pos + acc' < State.length input || more = Complete then
        succ input pos more acc'
      else
        let succ' input' pos' more' = (go acc').run input' pos' more' fail succ
        and fail' input' pos' more' = succ input' pos' more' acc' in
        prompt input pos fail' succ'
    }
  in
  go init

let string_ f s =
  (* XXX(seliopou): Inefficient. Could check prefix equality to short-circuit
   * the io. *)
  let len = String.length s in
  ensure len >>= fun s'->
    if f s = f s'
      then return s'
      else fail "string"

let string s    = string_ (fun x -> x) s
let string_ci s = string_ String.lowercase s

let skip_while f =
  count_while f >>= advance

let take n =
  ensure (max n 0)

let peek_string n =
  unsafe_lookahead (take n)

let take_while f =
  count_while f >>= unsafe_substring

let take_while1 f =
  count_while f
  >>= function
    | 0 -> fail "take_while1"
    | n -> unsafe_substring n

let take_till f =
  take_while (fun c -> not (f c))

let take_rest =
  let rec go acc =
    want_input >>= function
      | true  ->
        available >>= fun n ->
        unsafe_substring n >>= fun str ->
        go (str::acc)
      | false ->
        return (List.rev acc)
  in
  go []

let choice ps =
  List.fold_right (<|>) ps (fail "empty")

let fix f =
  let rec p = lazy (f r)
  and r = { run = fun buf pos more fail succ ->
    Lazy.(force p).run buf pos more fail succ }
  in
  r

let option x p =
  p <|> return x

let cons x xs = x :: xs

let rec list ps =
  match ps with
  | []    -> return []
  | p::ps -> lift2 cons p (list ps)

let count n p =
  if n < 0 then
    failwith "count: invalid argument, n < 0";
  let rec loop = function
    | 0 -> return []
    | n -> lift2 cons p (loop (n - 1))
  in
  loop n

let many p =
  fix (fun m ->
    (lift2 cons p m) <|> return [])

let many1 p =
  lift2 cons p (many p)

let many_till p t =
  fix (fun m ->
    (lift2 cons p m) <|> (t *> return []))

let sep_by1 s p =
  fix (fun m ->
    lift2 cons p ((s *> m) <|> return []))

let sep_by s p =
  (lift2 cons p ((s *> sep_by1 s p) <|> return [])) <|> return []

let skip_many p =
  fix (fun m ->
    (p *> m) <|> return ())

let skip_many1 p =
  p *> skip_many p

let end_of_line =
  (char '\n' *> return ()) <|> (string "\r\n" *> return ()) <?> "end_of_line"

module Make_endian(Es : EndianString.EndianStringSig) = struct
  let get_float s = Es.get_float s 0
  let get_double s = Es.get_double s 0

  let get_int8 s = Es.get_int8 s 0
  let get_int16 s = Es.get_int16 s 0
  let get_int32 s = Es.get_int32 s 0
  let get_int64 s = Es.get_int64 s 0

  let get_uint8 s = Es.get_uint8 s 0
  let get_uint16 s = Es.get_uint16 s 0
  let get_uint32 s = Es.get_int32 s 0
  let get_uint64 s = Es.get_int64 s 0

  (* int *)
  let uint8 =
    take 1 >>| get_uint8
  let uint16 =
    take 2 >>| get_uint16
  let uint32 =
    take 4 >>| get_uint32
  let uint64 =
    take 8 >>| get_uint64
  let int8 =
    take 1 >>| get_int8
  let int16 =
    take 2 >>| get_int16
  let int32 =
    take 4 >>| get_int32
  let int64 =
    take 8 >>| get_int64

  (* float *)
  let float =
    take 4 >>| get_float
  let double =
    take 8 >>| get_double
end

module Le = Make_endian(EndianString.LittleEndian_unsafe)
module Be = Make_endian(EndianString.BigEndian_unsafe)
module Ne = Make_endian(EndianString.NativeEndian_unsafe)
