(* TEST

   include dynlink
   files = "plugin.ml"

   * naked_pointers (* output is slightly but not meaningfully different in no-naked-pointers build *)
   ** shared-libraries
   *** setup-ocamlopt.opt-build-env
   **** ocamlopt.opt
   program2 = "testopt.exe"
   ***** ocamlopt.opt
   flags = "-shared"
   program2 = "plugin.cmxs"
   all_modules = "plugin.ml"
   ****** run
   program = "./testopt.exe"
   output = "${test_build_directory}/ocamlopt.opt.output"
   ******* no-flambda
   ******** not-windows
   ********* arch64
   ********** check-program-output
   reference = "${test_source_directory}/testopt64.reference"

   ********* arch32
   ********** check-program-output
   reference = "${test_source_directory}/testopt32.reference"

   ******* flambda
   ******** check-program-output
   reference = "${test_source_directory}/testoptflambda.reference"

   ******* windows
   ******** check-program-output
   reference = "${test_source_directory}/testoptwin.reference"
*)

let _ = Random.self_init
let rarray size = Array.init size (fun i -> Random.int 1000)
let sumarray array = Array.fold_left (fun a b -> a + b) 0 array

let u = rarray 10000
let v = rarray 20000

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

let () =
  try
    Dynlink.loadfile "plugin.cmxs"
  with
  | Dynlink.Error error ->
    prerr_endline (Dynlink.error_message error)

let f () = Gc.print_reachable 10000

let g () =
  let x = rarray 30000 in
  let y = rarray 40000 in
  let z = rarray 50000 in
  f ();
  (sumarray u) + (sumarray v) + (sumarray x) + (sumarray y) + (sumarray z)

let _ = g ()
