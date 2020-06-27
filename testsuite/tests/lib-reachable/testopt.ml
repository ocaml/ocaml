(* TEST
   * no-flambda
   ** not-windows
   *** arch64
   **** native
   reference = "${test_source_directory}/testopt64.reference"
   *** arch32
   **** native
   reference = "${test_source_directory}/testopt32.reference"
   * flambda
   ** native
   reference = "${test_source_directory}/testoptflambda.reference"
   * windows
   ** native
   reference = "${test_source_directory}/testoptwin.reference"
*)

(* todo: TEXSTXX

include dynlink
libraries = ""
files = "plugin.ml testopt.ml"

* native
** shared-libraries
*** setup-ocamlopt.opt-build-env
**** ocamlc.opt
program = "${test_build_directory}/testopt.opt"
flags = "-linkall"
include ocamlcommon
libraries += "dynlink"
all_modules = "testopt.ml"
***** run
******check-program-output
*)
(*
let _ =
  let test_src_dir = Sys.getenv("test_source_directory") in
  let flambda = try Sys.getenv("CONFIG_ARG") = "--enable-flambda" with Not_found -> false in
  let src = match flambda, Sys.unix, Sys.word_size with
    | true, _, _ -> "testoptflambda.reference"
    | _, true, 64 -> "testopt64.reference"
    | _, true, 32 -> "testopt32.reference"
    | _, false, _  -> "testwin.reference"
    | _, _, _ -> ""
  in
  let reference = Sys.getenv("reference") in
  Sys.command (Printf.sprintf "cp %s/%s %s" test_src_dir src reference)
*)

let _ = Random.self_init
let rarray size = Array.init size (fun i -> Random.int 1000)
let sumarray array = Array.fold_left (fun a b -> a + b) 0 array

let u = rarray 100
let v = rarray 200

module Foo : sig
  val junk : string list
  val junk2 : string list
end = struct
  let junk = ["abc"; "def" ^ "yz"]
  let junk2 = ["abc"; "def" ^ "yz"]
end

let field_path name var =
  let arr = Gc.field_path var in
  Printf.printf "Field path of %s =" name;
  Array.iter (fun item -> Printf.printf " %d" item) arr;
  Printf.printf "\n%!"

let _ = field_path "u" u
let _ = field_path "v" v

let _ = field_path "Foo.junk" Foo.junk
let _ = field_path "Foo.junk2" Foo.junk2

(*
let () =
  try
    Dynlink.loadfile "plugin"
  with
  | Dynlink.Error error ->
    prerr_endline (Dynlink.error_message error)
    let x = rArray 100
*)

let f () = Gc.print_reachable 100

let g () =
  let x = rarray 1000 in
  let y = rarray 2000 in
  let z = rarray 3000 in
  f ();
  (sumarray u) + (sumarray v) + (sumarray x) + (sumarray y) + (sumarray z)

let _ = g ()
