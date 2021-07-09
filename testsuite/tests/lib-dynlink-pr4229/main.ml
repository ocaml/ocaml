(* TEST

include dynlink

readonly_files = "abstract.mli abstract.ml static.ml client.ml main.ml"

subdirectories = "sub"

libraries = ""

* shared-libraries
** setup-ocamlc.byte-build-env
*** cd
cwd = "sub"
**** ocamlc.byte
module = "abstract.mli"
***** ocamlc.byte
module = "abstract.ml"
****** cd
cwd = ".."
******* ocamlc.byte
module = "abstract.mli"
******** ocamlc.byte
module = "abstract.ml"
********* ocamlc.byte
module = "static.ml"
********** ocamlc.byte
module = "client.ml"
*********** ocamlc.byte
module = "main.ml"
************ ocamlc.byte
program = "${test_build_directory}/main"
libraries = "dynlink"
module = ""
all_modules = "abstract.cmo static.cmo main.cmo"
************* run
exit_status = "2"
************** check-program-output

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** cd
cwd = "sub"
***** ocamlopt.byte
module = "abstract.mli"
****** ocamlopt.byte
program = "abstract.cmxs"
flags = "-shared"
module = ""
all_modules = "abstract.ml"
******* cd
cwd = ".."
******** ocamlopt.byte
flags = ""
module = "abstract.mli"
********* ocamlopt.byte
module = "abstract.ml"
********** ocamlopt.byte
module = "static.ml"
*********** ocamlopt.byte
program = "client.cmxs"
flags = "-shared"
module = ""
all_modules = "client.ml"
*********** ocamlopt.byte
module = "main.ml"
************ ocamlopt.byte
program = "${test_build_directory}/main_native"
libraries = "dynlink"
module = ""
all_modules = "abstract.cmx static.cmx main.cmx"
************* run
exit_status = "2"
************** check-program-output
*)

(* PR#4229 *)

let () =
  let suffix =
    match Sys.backend_type with
    | Native -> "cmxs"
    | Bytecode -> "cmo"
    | Other _ -> assert false
  in
  try
    (* Dynlink.init (); *)  (* this function has been removed from the API *)
    Dynlink.loadfile ("client."^suffix); (* utilise abstract.suffix *)
    Dynlink.loadfile ("sub/abstract."^suffix);
    Dynlink.loadfile ("client."^suffix) (* utilise sub/abstract.suffix *)
  with
  | Dynlink.Error (Dynlink.Module_already_loaded "Abstract") -> exit 2
