(* TEST

include dynlink
libraries = ""
readonly_files = "store.ml run.ml link01.ml link02.ml link03.ml link04.ml link05.ml link06.ml link07.ml"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "store.ml"
**** ocamlc.byte
module = "link01.ml"
***** ocamlc.byte
module = "link02.ml"
****** ocamlc.byte
module = "link03.ml"
******* ocamlc.byte
module = "link04.ml"
******** ocamlc.byte
module = "link05.ml"
********* ocamlc.byte
module = "link06.ml"
********** ocamlc.byte
module = "link07.ml"
*********** ocamlc.byte
module = "run.ml"
************ ocamlc.byte
program = "./run.byte.exe"
libraries="dynlink"
all_modules = "store.cmo run.cmo"
module = ""
************* run
************** check-program-output


** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
flags = ""
module = "store.ml"
***** ocamlopt.byte
program= "link01.cmxs"
flags = "-shared"
module = ""
all_modules = "link01.ml"
****** ocamlopt.byte
flags = "-shared"
program= "link02.cmxs"
modules = ""
all_modules = "link02.ml"
******* ocamlopt.byte
flags = "-shared"
program= "link03.cmxs"
modules = ""
all_modules = "link03.ml"
******** ocamlopt.byte
flags = "-shared"
program = "link04.cmxs"
modules = ""
all_modules = "link04.ml"
********* ocamlopt.byte
flags = "-shared"
program = "link05.cmxs"
modules = ""
all_modules = "link05.ml"
********** ocamlopt.byte
flags = "-shared"
program = "link06.cmxs"
modules = ""
all_modules = "link06.ml"
*********** ocamlopt.byte
flags = "-shared"
program = "link07.cmxs"
modules = ""
all_modules = "link07.ml"
************ ocamlopt.byte
flags = ""
module = "run.ml"
************* ocamlopt.byte
program = "./runner.exe"
libraries="dynlink"
all_modules = "store.cmx run.cmx"
module = ""
************** run
*************** check-program-output


*)



let range = List.init 7 Fun.id

let dlinks =
  let name i = Dynlink.adapt_filename @@ Printf.sprintf "link%02d.cmo" (i+1) in
  List.map
    (fun i -> Domain.spawn (fun () -> Dynlink.loadfile @@ name i ))
    range

let () = List.iter Domain.join dlinks

module String_set = Set.Make(String)


let stored = Atomic.get Store.store
let stored_set =
  List.fold_left (fun s x -> String_set.add x s) String_set.empty stored

let expected = List.fold_left (fun s i ->
    String_set.add (Printf.sprintf "Link%d" (i+1)) s
  ) String_set.empty range

let () =
  let () = List.iter (Printf.printf "%s\n") (String_set.elements stored_set) in
  assert (String_set.equal stored_set expected)
