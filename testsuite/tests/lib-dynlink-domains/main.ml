(* TEST

include dynlink
libraries = ""
readonly_files = "store.ml main.ml Plugin_0.ml Plugin_0_0.ml Plugin_0_0_0.ml Plugin_0_0_0_0.ml Plugin_0_1.ml Plugin_0_1_0.ml Plugin_0_1_0_0.ml Plugin_0_2.ml Plugin_0_2_0.ml Plugin_0_2_0_0.ml Plugin_0_2_1.ml Plugin_0_2_1_0.ml Plugin_0_2_2.ml Plugin_0_2_2_0.ml Plugin_0_2_3.ml Plugin_0_2_3_0.ml Plugin_1.ml Plugin_1_0.ml Plugin_1_0_0.ml Plugin_1_0_0_0.ml Plugin_1_1.ml Plugin_1_2.ml Plugin_1_2_0.ml Plugin_1_2_0_0.ml Plugin_1_3.ml Plugin_2.ml Plugin_2_0.ml Plugin_2_0_0.ml Plugin_2_0_0_0.ml Plugin_3.ml Plugin_3_0.ml Plugin_3_1.ml Plugin_3_2.ml Plugin_3_2_0.ml Plugin_3_2_0_0.ml Plugin_3_2_1.ml Plugin_3_2_2.ml Plugin_3_2_2_0.ml Plugin_3_2_2_1.ml"

*01 shared-libraries
*02 setup-ocamlc.byte-build-env
*03 ocamlc.byte
module = "store.ml"
*04 ocamlc.byte
module = "Plugin_0.ml"
*05 ocamlc.byte
module = "Plugin_0_0.ml"
*06 ocamlc.byte
module = "Plugin_0_0_0.ml"
*07 ocamlc.byte
module = "Plugin_0_0_0_0.ml"
*08 ocamlc.byte
module = "Plugin_0_1.ml"
*09 ocamlc.byte
module = "Plugin_0_1_0.ml"
*10 ocamlc.byte
module = "Plugin_0_1_0_0.ml"
*11 ocamlc.byte
module = "Plugin_0_2.ml"
*12 ocamlc.byte
module = "Plugin_0_2_0.ml"
*13 ocamlc.byte
module = "Plugin_0_2_0_0.ml"
*14 ocamlc.byte
module = "Plugin_0_2_1.ml"
*15 ocamlc.byte
module = "Plugin_0_2_1_0.ml"
*16 ocamlc.byte
module = "Plugin_0_2_2.ml"
*17 ocamlc.byte
module = "Plugin_0_2_2_0.ml"
*18 ocamlc.byte
module = "Plugin_0_2_3.ml"
*19 ocamlc.byte
module = "Plugin_0_2_3_0.ml"
*20 ocamlc.byte
module = "Plugin_1.ml"
*21 ocamlc.byte
module = "Plugin_1_0.ml"
*22 ocamlc.byte
module = "Plugin_1_0_0.ml"
*23 ocamlc.byte
module = "Plugin_1_0_0_0.ml"
*24 ocamlc.byte
module = "Plugin_1_1.ml"
*25 ocamlc.byte
module = "Plugin_1_2.ml"
*26 ocamlc.byte
module = "Plugin_1_2_0.ml"
*27 ocamlc.byte
module = "Plugin_1_2_0_0.ml"
*28 ocamlc.byte
module = "Plugin_1_3.ml"
*29 ocamlc.byte
module = "Plugin_2.ml"
*30 ocamlc.byte
module = "Plugin_2_0.ml"
*31 ocamlc.byte
module = "Plugin_2_0_0.ml"
*32 ocamlc.byte
module = "Plugin_2_0_0_0.ml"
*33 ocamlc.byte
module = "Plugin_3.ml"
*34 ocamlc.byte
module = "Plugin_3_0.ml"
*35 ocamlc.byte
module = "Plugin_3_1.ml"
*36 ocamlc.byte
module = "Plugin_3_2.ml"
*37 ocamlc.byte
module = "Plugin_3_2_0.ml"
*38 ocamlc.byte
module = "Plugin_3_2_0_0.ml"
*39 ocamlc.byte
module = "Plugin_3_2_1.ml"
*40 ocamlc.byte
module = "Plugin_3_2_2.ml"
*41 ocamlc.byte
module = "Plugin_3_2_2_0.ml"
*42 ocamlc.byte
module = "Plugin_3_2_2_1.ml"
*43 ocamlc.byte
module = "main.ml"
*44 ocamlc.byte
program = "./main.byte.exe"
libraries= "dynlink"
all_modules = "store.cmo main.cmo"
module = ""
*45 run
*46 check-program-output

*02 native-dynlink
*03 setup-ocamlopt.byte-build-env
*04 ocamlopt.byte
flags = ""
module = "store.ml"
*05 ocamlopt.byte
flags = "-shared"
program= "Plugin_0.cmxs"
module = ""
all_modules = "Plugin_0.ml"
*06 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0.ml"
*07 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0_0.ml"
*08 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_0_0_0.cmxs"
module = ""
all_modules = "Plugin_0_0_0_0.ml"
*09 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1.cmxs"
module = ""
all_modules = "Plugin_0_1.ml"
*10 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1_0.cmxs"
module = ""
all_modules = "Plugin_0_1_0.ml"
*11 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_1_0_0.cmxs"
module = ""
all_modules = "Plugin_0_1_0_0.ml"
*12 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2.cmxs"
module = ""
all_modules = "Plugin_0_2.ml"
*13 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_0.cmxs"
module = ""
all_modules = "Plugin_0_2_0.ml"
*14 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_0_0.cmxs"
module = ""
all_modules = "Plugin_0_2_0_0.ml"
*15 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_1.cmxs"
module = ""
all_modules = "Plugin_0_2_1.ml"
*16 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_1_0.cmxs"
module = ""
all_modules = "Plugin_0_2_1_0.ml"
*17 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_2.cmxs"
module = ""
all_modules = "Plugin_0_2_2.ml"
*18 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_2_0.cmxs"
module = ""
all_modules = "Plugin_0_2_2_0.ml"
*19 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_3.cmxs"
module = ""
all_modules = "Plugin_0_2_3.ml"
*20 ocamlopt.byte
flags = "-shared"
program= "Plugin_0_2_3_0.cmxs"
module = ""
all_modules = "Plugin_0_2_3_0.ml"
*21 ocamlopt.byte
flags = "-shared"
program= "Plugin_1.cmxs"
module = ""
all_modules = "Plugin_1.ml"
*22 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0.cmxs"
module = ""
all_modules = "Plugin_1_0.ml"
*23 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0_0.cmxs"
module = ""
all_modules = "Plugin_1_0_0.ml"
*24 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_0_0_0.cmxs"
module = ""
all_modules = "Plugin_1_0_0_0.ml"
*25 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_1.cmxs"
module = ""
all_modules = "Plugin_1_1.ml"
*26 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2.cmxs"
module = ""
all_modules = "Plugin_1_2.ml"
*27 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2_0.cmxs"
module = ""
all_modules = "Plugin_1_2_0.ml"
*28 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_2_0_0.cmxs"
module = ""
all_modules = "Plugin_1_2_0_0.ml"
*29 ocamlopt.byte
flags = "-shared"
program= "Plugin_1_3.cmxs"
module = ""
all_modules = "Plugin_1_3.ml"
*30 ocamlopt.byte
flags = "-shared"
program= "Plugin_2.cmxs"
module = ""
all_modules = "Plugin_2.ml"
*31 ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0.cmxs"
module = ""
all_modules = "Plugin_2_0.ml"
*32 ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0_0.cmxs"
module = ""
all_modules = "Plugin_2_0_0.ml"
*33 ocamlopt.byte
flags = "-shared"
program= "Plugin_2_0_0_0.cmxs"
module = ""
all_modules = "Plugin_2_0_0_0.ml"
*34 ocamlopt.byte
flags = "-shared"
program= "Plugin_3.cmxs"
module = ""
all_modules = "Plugin_3.ml"
*35 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_0.cmxs"
module = ""
all_modules = "Plugin_3_0.ml"
*36 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_1.cmxs"
module = ""
all_modules = "Plugin_3_1.ml"
*37 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2.cmxs"
module = ""
all_modules = "Plugin_3_2.ml"
*38 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_0.cmxs"
module = ""
all_modules = "Plugin_3_2_0.ml"
*39 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_0_0.cmxs"
module = ""
all_modules = "Plugin_3_2_0_0.ml"
*40 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_1.cmxs"
module = ""
all_modules = "Plugin_3_2_1.ml"
*41 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2.cmxs"
module = ""
all_modules = "Plugin_3_2_2.ml"
*42 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2_0.cmxs"
module = ""
all_modules = "Plugin_3_2_2_0.ml"
*43 ocamlopt.byte
flags = "-shared"
program= "Plugin_3_2_2_1.cmxs"
module = ""
all_modules = "Plugin_3_2_2_1.ml"
*44 ocamlopt.byte
flags = ""
module = "main.ml"
*45 ocamlopt.byte
program = "./main.exe"
libraries="dynlink"
all_modules = "store.cmx main.cmx"
module = ""
*46 run
*47 check-program-output
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
