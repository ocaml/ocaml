(* TEST
   flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/toplevel"
   include ocamlcommon
   * expect
*)

(* Check that [e.pexp_loc :: e.pexp_loc_stack] includes all
   intermediate locations of an expression. *)

let blocks =
  let s = {| ( (* comment *) (( "contant" [@attr] )  (* comment *))) |} in
  let e = Parse.expression (Lexing.from_string s) in
  let extract (loc : Location.t) =
    let a = loc.loc_start.pos_cnum in
    let b = loc.loc_end.pos_cnum in
    String.sub s a (b - a)
  in
  List.map extract (e.pexp_loc :: e.pexp_loc_stack)
;;
[%%expect {|
val blocks : string list =
  ["( (* comment *) (( \"contant\" [@attr] )  (* comment *)))";
   "(( \"contant\" [@attr] )  (* comment *))"; "( \"contant\" [@attr] )";
   "\"contant\""]
|}];;
