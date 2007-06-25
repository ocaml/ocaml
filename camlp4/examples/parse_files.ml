open Camlp4.PreCast;;

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
      (Camlp4OCamlRevisedParser.Make
        (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))));;

let parse f =
  let ic = open_in f in
  let strm = Stream.of_channel ic in
  let res = Caml.parse_implem (Loc.mk f) strm in
  close_in ic; res;;

let ghost = Loc.ghost;;

let main () =
  let a = parse "apply_operator_test.ml" in
  let b = parse "global_handler_test.ml" in
  Caml.print_implem
    <:str_item@ghost<
      module Apply_operator_test = struct $a$ end;;
      module Global_handler_test = struct $b$ end >>
;;

try main ()
with e ->
  Format.eprintf "error: %a@." Camlp4.ErrorHandler.print e;
  exit 1;;
