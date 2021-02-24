(* TEST
   flags += " -w a "
   modules = "globrootsprim.c globroot.ml"
*)

open Globroot

module TestClassic = Test(Classic)
module TestGenerational = Test(Generational)

let n = 50

let _ =
  for _ = 1 to 20 do
    let burn = fun () -> TestClassic.test n; TestGenerational.test n in
    let d = Array.init 4 (fun _ -> Domain.spawn burn) in
    Array.iter Domain.join d
  done;
  print_string "ok\n"