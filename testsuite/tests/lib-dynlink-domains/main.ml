(* TEST

include dynlink
libraries = ""
readonly_files = "store.ml main.ml Plugin_0.ml Plugin_0_0.ml Plugin_0_0_0.ml Plugin_0_0_0_0.ml Plugin_0_1.ml Plugin_0_1_0.ml Plugin_0_1_0_0.ml Plugin_0_2.ml Plugin_0_2_0.ml Plugin_0_2_0_0.ml Plugin_0_2_1.ml Plugin_0_2_1_0.ml Plugin_0_2_2.ml Plugin_0_2_2_0.ml Plugin_0_2_3.ml Plugin_0_2_3_0.ml Plugin_1.ml Plugin_1_0.ml Plugin_1_0_0.ml Plugin_1_0_0_0.ml Plugin_1_1.ml Plugin_1_2.ml Plugin_1_2_0.ml Plugin_1_2_0_0.ml Plugin_1_3.ml Plugin_2.ml Plugin_2_0.ml Plugin_2_0_0.ml Plugin_2_0_0_0.ml Plugin_3.ml Plugin_3_0.ml Plugin_3_1.ml Plugin_3_2.ml Plugin_3_2_0.ml Plugin_3_2_0_0.ml Plugin_3_2_1.ml Plugin_3_2_2.ml Plugin_3_2_2_0.ml Plugin_3_2_2_1.ml"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
module = "store.ml"
**** ocamlc.byte
module = "Plugin_0.ml"
***** ocamlc.byte
module = "Plugin_0_0.ml"
****** ocamlc.byte
module = "Plugin_0_0_0.ml"
******* ocamlc.byte
module = "Plugin_0_0_0_0.ml"
******** ocamlc.byte
module = "Plugin_0_1.ml"
********* ocamlc.byte
module = "Plugin_0_1_0.ml"
********** ocamlc.byte
module = "Plugin_0_1_0_0.ml"
*********** ocamlc.byte
module = "Plugin_0_2.ml"
************ ocamlc.byte
module = "Plugin_0_2_0.ml"
************* ocamlc.byte
module = "Plugin_0_2_0_0.ml"
************** ocamlc.byte
module = "Plugin_0_2_1.ml"
*************** ocamlc.byte
module = "Plugin_0_2_1_0.ml"
**************** ocamlc.byte
module = "Plugin_0_2_2.ml"
***************** ocamlc.byte
module = "Plugin_0_2_2_0.ml"
****************** ocamlc.byte
module = "Plugin_0_2_3.ml"
******************* ocamlc.byte
module = "Plugin_0_2_3_0.ml"
******************** ocamlc.byte
module = "Plugin_1.ml"
********************* ocamlc.byte
module = "Plugin_1_0.ml"
********************** ocamlc.byte
module = "Plugin_1_0_0.ml"
*********************** ocamlc.byte
module = "Plugin_1_0_0_0.ml"
************************ ocamlc.byte
module = "Plugin_1_1.ml"
************************* ocamlc.byte
module = "Plugin_1_2.ml"
************************** ocamlc.byte
module = "Plugin_1_2_0.ml"
*************************** ocamlc.byte
module = "Plugin_1_2_0_0.ml"
**************************** ocamlc.byte
module = "Plugin_1_3.ml"
***************************** ocamlc.byte
module = "Plugin_2.ml"
****************************** ocamlc.byte
module = "Plugin_2_0.ml"
******************************* ocamlc.byte
module = "Plugin_2_0_0.ml"
******************************** ocamlc.byte
module = "Plugin_2_0_0_0.ml"
********************************* ocamlc.byte
module = "Plugin_3.ml"
********************************** ocamlc.byte
module = "Plugin_3_0.ml"
*********************************** ocamlc.byte
module = "Plugin_3_1.ml"
************************************ ocamlc.byte
module = "Plugin_3_2.ml"
************************************* ocamlc.byte
module = "Plugin_3_2_0.ml"
************************************** ocamlc.byte
module = "Plugin_3_2_0_0.ml"
*************************************** ocamlc.byte
module = "Plugin_3_2_1.ml"
**************************************** ocamlc.byte
module = "Plugin_3_2_2.ml"
***************************************** ocamlc.byte
module = "Plugin_3_2_2_0.ml"
****************************************** ocamlc.byte
module = "Plugin_3_2_2_1.ml"
******************************************* ocamlc.byte
module = "main.ml"
******************************************** ocamlc.byte
program = "./main.byte.exe"
libraries= "dynlink"
all_modules = "store.cmo main.cmo"
module = ""
********************************************* run
********************************************** check-program-output

** native-dynlink
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
flags = ""
module = "store.ml"
***** ocamlopt.byte
flags = "-shared"
program= "Plugin_0.cmxs"
module = ""
all_modules = "Plugin_0.ml"
****** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0.ml"
******* ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0_0.ml"
******** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0_0_0.ml"
********* ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1.cmxs"
module = ""
all_modules = "Plugin_0_1.ml"
********** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1_0.cmxs"
module = ""
all_modules = "Plugin_0_1_0.ml"
*********** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1_0_0.cmxs"
module = ""
all_modules = "Plugin_0_1_0_0.ml"
************ ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2.cmxs"
module = ""
all_modules = "Plugin_0_2.ml"
************* ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_0.cmxs"
module = ""
all_modules = "Plugin_0_2_0.ml"
************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_0_0.cmxs"
module = ""
all_modules = "Plugin_0_2_0_0.ml"
*************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_1.cmxs"
module = ""
all_modules = "Plugin_0_2_1.ml"
**************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_1_0.cmxs"
module = ""
all_modules = "Plugin_0_2_1_0.ml"
***************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_2.cmxs"
module = ""
all_modules = "Plugin_0_2_2.ml"
****************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_2_0.cmxs"
module = ""
all_modules = "Plugin_0_2_2_0.ml"
******************* ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_3.cmxs"
module = ""
all_modules = "Plugin_0_2_3.ml"
******************** ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_3_0.cmxs"
module = ""
all_modules = "Plugin_0_2_3_0.ml"
********************* ocamlopt.byte
flags = "-shared"
program= "Plugin_1.cmxs"
module = ""
all_modules = "Plugin_1.ml"
********************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0.cmxs"
module = ""
all_modules = "Plugin_1_0.ml"
*********************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0_0.cmxs"
module = ""
all_modules = "Plugin_1_0_0.ml"
************************ ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0_0_0.cmxs"
module = ""
all_modules = "Plugin_1_0_0_0.ml"
************************* ocamlopt.byte
flags = "-shared"
program= "Plugin_1_1.cmxs"
module = ""
all_modules = "Plugin_1_1.ml"
************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2.cmxs"
module = ""
all_modules = "Plugin_1_2.ml"
*************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2_0.cmxs"
module = ""
all_modules = "Plugin_1_2_0.ml"
**************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2_0_0.cmxs"
module = ""
all_modules = "Plugin_1_2_0_0.ml"
***************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_1_3.cmxs"
module = ""
all_modules = "Plugin_1_3.ml"
****************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_2.cmxs"
module = ""
all_modules = "Plugin_2.ml"
******************************* ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0.cmxs"
module = ""
all_modules = "Plugin_2_0.ml"
******************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0_0.cmxs"
module = ""
all_modules = "Plugin_2_0_0.ml"
********************************* ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0_0_0.cmxs"
module = ""
all_modules = "Plugin_2_0_0_0.ml"
********************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3.cmxs"
module = ""
all_modules = "Plugin_3.ml"
*********************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_0.cmxs"
module = ""
all_modules = "Plugin_3_0.ml"
************************************ ocamlopt.byte
flags = "-shared"
program= "Plugin_3_1.cmxs"
module = ""
all_modules = "Plugin_3_1.ml"
************************************* ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2.cmxs"
module = ""
all_modules = "Plugin_3_2.ml"
************************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_0.cmxs"
module = ""
all_modules = "Plugin_3_2_0.ml"
*************************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_0_0.cmxs"
module = ""
all_modules = "Plugin_3_2_0_0.ml"
**************************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_1.cmxs"
module = ""
all_modules = "Plugin_3_2_1.ml"
***************************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2.cmxs"
module = ""
all_modules = "Plugin_3_2_2.ml"
****************************************** ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2_0.cmxs"
module = ""
all_modules = "Plugin_3_2_2_0.ml"
******************************************* ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2_1.cmxs"
module = ""
all_modules = "Plugin_3_2_2_1.ml"
******************************************** ocamlopt.byte
flags = ""
module = "main.ml"
********************************************* ocamlopt.byte
program = "./main.exe"
libraries="dynlink"
all_modules = "store.cmx main.cmx"
module = ""
********************************************** run
*********************************************** check-program-output
*)

(*  This module and all plugin modules are generated by a call to test_generator.ml with parameters:
seed=20, width=20, depth=4, nlinks=2, introns=8, childs=4, domains=20.
*)
(* Link plugins *)
let d0 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_0.cmo")
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.c6143fe60e096p+1
let () = Store.add "[]->[]"
let d1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1.cmo")
let () = Store.add "[]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.61cd1249a1f8ap+1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 879." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.41e1b93de6bc2p+1
let d2 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_2.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 837." ^ "That's all"
let d3 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_3.cmo")
let () = Domain.join d0
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 0." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 673." ^ "That's all"
let () = Domain.join d1
let add x = Store.add x
let () = Domain.join d3
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.2ef627b0c12f6p+2
let () = Domain.join d2

(* Print result *)

module String_set = Set.Make(String)
let stored = Atomic.get Store.store
let stored_set = String_set.of_list stored
let () =
  List.iter (Printf.printf "%s\n") (String_set.elements stored_set)
