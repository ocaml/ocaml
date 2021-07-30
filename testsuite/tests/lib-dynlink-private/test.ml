(* TEST

include dynlink
libraries = ""
readonly_files = "sheep.mli sheep.ml pig.mli"
subdirectories = "plugin1 plugin2 plugin2b plugin2c plugin3 plugin4 \
  plugin5 plugin6"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "sheep.mli"
**** ocamlc.byte
module = "sheep.ml"
***** ocamlc.byte
module = "pig.mli"
****** ocamlc.byte
module = "test.ml"
******* ocamlc.byte
module = "plugin1/sheep.mli"
******** ocamlc.byte
flags = "-I plugin1"
module = "plugin1/sheep.ml"
********* ocamlc.byte
flags = ""
module = "plugin2/cow.mli"
********** ocamlc.byte
flags = "-I plugin2"
module = "plugin2/cow.ml"
*********** ocamlc.byte
flags = ""
module = "plugin2b/cow.mli"
************ ocamlc.byte
flags = "-I plugin2b"
module = "plugin2b/cow.ml"
************* ocamlc.byte
flags = ""
module = "plugin2c/cow.mli"
************** ocamlc.byte
flags = "-I plugin2c"
module = "plugin2c/cow.ml"
*************** ocamlc.byte
flags = ""
module = "plugin3/pig.mli"
**************** ocamlc.byte
flags = "-I plugin3"
module = "plugin3/pig.ml"
***************** ocamlc.byte
flags = ""
module = "plugin4/chicken.mli"
****************** ocamlc.byte
flags = "-I plugin4"
module = "plugin4/chicken.ml"
******************* ocamlc.byte
flags = ""
module = "plugin5/chicken.mli"
******************** ocamlc.byte
flags = "-I plugin5"
module = "plugin5/chicken.ml"
********************* ocamlc.byte
flags = ""
module = "plugin6/pheasant.mli"
********************** ocamlc.byte
flags = "-I plugin6"
module = "plugin6/pheasant.ml"
*********************** ocamlc.byte
flags = ""
module = "plugin6/partridge.mli"
************************ ocamlc.byte
flags = "-I plugin6"
module = "plugin6/partridge.ml"
************************* ocamlc.byte
flags = ""
program = "./test.byte.exe"
libraries = "dynlink"
all_modules = "sheep.cmo test.cmo"
module = ""
************************** run

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
module = "sheep.mli"
***** ocamlopt.byte
module = "sheep.ml"
****** ocamlopt.byte
module = "pig.mli"
******* ocamlopt.byte
module = "test.ml"
******** ocamlopt.byte
flags = ""
module = "plugin1/sheep.mli"
********* ocamlopt.byte
program = "plugin1/sheep.cmxs"
flags = "-I plugin1 -shared"
module = ""
all_modules = "plugin1/sheep.ml"
********** ocamlopt.byte
flags = ""
module = "plugin2/cow.mli"
*********** ocamlopt.byte
program = "plugin2/cow.cmxs"
flags = "-I plugin2 -shared"
module = ""
all_modules = "plugin2/cow.ml"
************ ocamlopt.byte
flags = ""
module = "plugin2b/cow.mli"
************* ocamlopt.byte
program = "plugin2b/cow.cmxs"
flags = "-I plugin2b -shared"
module = ""
all_modules = "plugin2b/cow.ml"
************** ocamlopt.byte
flags = ""
module = "plugin2c/cow.mli"
*************** ocamlopt.byte
program = "plugin2c/cow.cmxs"
flags = "-I plugin2c -shared"
module = ""
all_modules = "plugin2c/cow.ml"
**************** ocamlopt.byte
flags = ""
module = "plugin3/pig.mli"
***************** ocamlopt.byte
program = "plugin3/pig.cmxs"
flags = "-I plugin3 -shared"
module = ""
all_modules = "plugin3/pig.ml"
****************** ocamlopt.byte
flags = ""
module = "plugin4/chicken.mli"
******************* ocamlopt.byte
program = "plugin4/chicken.cmxs"
flags = "-I plugin4 -shared"
module = ""
all_modules = "plugin4/chicken.ml"
******************** ocamlopt.byte
flags = ""
module = "plugin5/chicken.mli"
********************* ocamlopt.byte
program = "plugin5/chicken.cmxs"
flags = "-I plugin5 -shared"
module = ""
all_modules = "plugin5/chicken.ml"
********************** ocamlopt.byte
flags = ""
module = "plugin6/pheasant.mli"
*********************** ocamlopt.byte
program = "plugin6/pheasant.cmxs"
flags = "-I plugin6 -shared"
module = ""
all_modules = "plugin6/pheasant.ml"
************************ ocamlopt.byte
flags = ""
module = "plugin6/partridge.mli"
************************* ocamlopt.byte
program = "plugin6/partridge.cmxs"
flags = "-I plugin6 -shared"
module = ""
all_modules = "plugin6/partridge.ml"
************************** ocamlopt.byte
flags = ""
program = "./test.opt.exe"
libraries = "dynlink"
all_modules = "sheep.cmx test.cmx"
*************************** run
*)

let () = Sheep.baa Sheep.s (* Use Sheep module *)
let _ = fun (x : Pig.t) -> x (* Reference Pig module *)

(* Test that a privately loaded module cannot have the same name as a
   module in the program. *)
let test_sheep () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin1/sheep.cmxs"
    else
      Dynlink.loadfile_private "plugin1/sheep.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Module_already_loaded "Sheep") -> ()

(* Test repeated loading of a privately-loaded module. *)
let test_cow_repeated () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2/cow.cmo"

(* Test that a privately loaded module can have the same name as a
   previous privately loaded module, in the case where the interfaces are
   the same, but the implementations differ. *)
let test_cow_same_name_same_mli () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2b/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2b/cow.cmo"

(* Test that a privately loaded module can have the same name as a
   previous privately loaded module, in the case where neither the interfaces
   nor the implementations are the same. *)
let test_cow_same_name_different_mli () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2c/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2c/cow.cmo"

(* Test that a privately loaded module cannot have the same name as an
   interface depended on by modules the program. *)
let test_pig () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin3/pig.cmxs"
    else
      Dynlink.loadfile_private "plugin3/pig.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Private_library_cannot_implement_interface "Pig") -> ()

(* Test that a privately loaded module can recursively load a module of
   the same name. *)
let test_chicken () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin4/chicken.cmxs"
  else
    Dynlink.loadfile_private "plugin4/chicken.cmo"

(* Test that a public load of a module M inside a privately-loaded module,
   followed by a public load of M, causes an error. *)
let test_pheasant () =
  begin
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin6/pheasant.cmxs"
    else
      Dynlink.loadfile_private "plugin6/pheasant.cmo"
  end;
  match
    if Dynlink.is_native then
      Dynlink.loadfile "plugin6/partridge.cmxs"
    else
      Dynlink.loadfile "plugin6/partridge.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Module_already_loaded "Partridge") -> ()

let () =
  test_sheep ();
  test_cow_repeated ();
  test_cow_repeated ();
  test_cow_same_name_same_mli ();
  test_cow_same_name_different_mli ();
  test_pig ();
  test_chicken ();
  test_pheasant ()
