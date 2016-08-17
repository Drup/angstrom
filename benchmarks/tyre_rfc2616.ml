
(* HTTP "parser", copied from angstrom. *)

open Tyre

module P = struct
  let space = Re.blank

  let eol = Re.set "\r\n"

  let token =
    Re.(rep1 @@ compl [
        rg '\000' '\031' ;
        set "\127)(<>@,;:\\/[]?={}"
      ])
end

let take_till re = regex @@ Re.(rep @@ compl [re])

let token = regex P.token
let digits = pos_int
let spaces = regex @@ Re.rep P.space

let lex p = p <** (spaces, "")

let version =
  "HTTP/" *>
    seq
      (digits <* ".")
      digits

let uri =
  take_till P.space

let meth = token
let eol = "\r\n"

let request_first_line =
    lex meth <*> lex uri <*> version

let response_first_line =
    (lex version) <*>
    (lex (take_till P.space)) <*>
    (take_till P.eol)

let header =
  let colon = regex @@ Re.(seq [char ':'; rep blank]) in
  seq
    token
    ((colon,":") **> take_till P.eol)


let request =
  let to_ (((meth, uri), version), headers) = Some (meth, uri, version, headers) in
  let of_ (meth, uri, version, headers) = (((meth, uri), version), headers) in
  conv ~name:"request" to_ of_ @@ seq
    (request_first_line   <* eol)
    (list (header <* eol) <* eol)

let response =
  let to_ (((version, status), msg), headers) = Some (version, status, msg, headers) in
  let of_ (version, status, msg, headers) = (((version, status), msg), headers) in
  conv ~name:"response" to_ of_ @@ seq
    (response_first_line  <* eol)
    (list (header <* eol) <* eol)
