(* Auxiliary functions for parsing *)

type error =
    Unbound of string

exception Error of error

let tbl_ident = (Hashtbl.new 57 : (string, Ident.t) Hashtbl.t)

let bind_ident s =
  let id = Ident.new s in
  Hashtbl.add tbl_ident s id;
  id

let find_ident s =
  try
    Hashtbl.find tbl_ident s
  with Not_found ->
    raise(Error(Unbound s))

let unbind_ident id =
  Hashtbl.remove tbl_ident (Ident.name id)

let report_error = function
    Unbound s ->
      prerr_string "Unbound identifier "; prerr_string s; prerr_endline "."
