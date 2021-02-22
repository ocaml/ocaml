(* TEST
   flags += " -w a "
   modules = "globrootsprim.c globroot.ml"
*)
open Globroot

let num_domains = 4
let n = 1000
let v = ref 0

let _ =
  let domains = Array.init (num_domains - 1) (fun _ -> 
    Domain.spawn(fun () -> 
      while(!v = 0) do
      1 + 1 |> ignore
      done
      )) in
  young2old (); Gc.full_major ();
  assert (static2young (1, 1) Gc.full_major == 0x42);
  TestClassic.test n;
  TestGenerational.test n;
  v := 1;
  Array.iter Domain.join domains;
  print_string "ok\n"
