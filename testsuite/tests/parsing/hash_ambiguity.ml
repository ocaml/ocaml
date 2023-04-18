(* TEST_BELOW
Lorem_ipsum_dolor_sit_amet,consectetur
_adipiscing_elit.Sed_non_risus.L
orem_ipsum_dolor_
sit_amet,consectetur_adipiscing
*)

class ['a] list = object end
type 'a t = int #list as 'a
  (* Here, "int #list" must be understood as a type.
     Another interpretation would be to understand just "int"
     as a type and view "#list" as a toplevel directive.
     A syntax error would then be reported at "as". *)

type 'a u = A of int #list

type 'a v = A of int * int #list

(* TEST
{
  flags = "-stop-after parsing -dparsetree";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
