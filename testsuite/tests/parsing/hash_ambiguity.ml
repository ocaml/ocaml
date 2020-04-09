(* TEST
   flags = "-stop-after parsing -dparsetree"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

class ['a] list = object end
type 'a t = int #list as 'a
  (* Here, "int #list" must be understood as a type.
     Another interpretation would be to understand just "int"
     as a type and view "#list" as a toplevel directive.
     A syntax error would then be reported at "as". *)

type 'a u = A of int #list

type 'a v = A of int * int #list
