(* TEST
 no-tsan; (* (Probably spurious) TSan alarm on this test, see
              https://github.com/ocaml/ocaml/issues/14300 *)
 {
   bytecode;
 }
 {
   native;
 }
*)

module M = Gc.Memprof

let start alloc_minor = ignore (M.start ~sampling_rate:1. { M.null_tracker with alloc_minor })

let alloc_some () =
  let rec f n =
    if n = 0 then [] else (ref 0) :: (f (n-1))
  in
  ignore (Sys.opaque_identity (f 100))

let is_sampling () =
  Printf.printf "domain %n is_sampling (): %b.\n"
    (Domain.self () :> int)
    (M.is_sampling ())

let dom1_witness = Atomic.make 0
let dom2_witness = Atomic.make 0

let print_witnesses () =
  Printf.printf "%n, %n\n" (Atomic.get dom1_witness) (Atomic.get dom2_witness)

let dom1_callback _ =
  let () =
    let i = Atomic.get dom1_witness in
    if i = 0 then
      Atomic.set dom1_witness 1
    else if i = 1 && Atomic.get dom2_witness = 1 then
      Atomic.set dom1_witness 2
  in
  None

let dom2_callback _ =
  let () =
    if Atomic.get dom1_witness = 1 && Atomic.get dom2_witness = 0 then
      Atomic.set dom2_witness 1
  in
  None

let _ =
  is_sampling () ;
  start dom1_callback ;
  is_sampling () ;
  ignore (alloc_some ()) ;
  let d = Domain.spawn (fun () ->
    is_sampling () ;
    start dom2_callback ;
    is_sampling () ;
    alloc_some () ;
    M.stop () ;
    is_sampling ())
  in
  Domain.join d ;
  is_sampling () ;
  alloc_some () ;
  print_witnesses ()
