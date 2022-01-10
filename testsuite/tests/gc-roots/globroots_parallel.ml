(* TEST
   flags += " -w a "
   modules = "globrootsprim.c globroots.ml"
*)

open Globroots

let num_domains = 8
let n = 125

let _ =
  let domains = Array.init (num_domains - 1) (fun _ ->
    Domain.spawn(fun () ->
      let module TestClassic = Test(Classic) () in
      let module TestGenerational = Test(Generational) () in
      TestClassic.test n;
      TestGenerational.test n)) in
  young2old (); Gc.full_major ();
  assert (static2young (1, 1) Gc.full_major == 0x42);
  TestClassic.test n;
  TestGenerational.test n;
  Array.iter Domain.join domains;
  print_string "ok\n"
