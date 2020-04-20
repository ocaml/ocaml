(* TEST
  * arch_power
  ** native
  *** ocamlopt.byte
  ocamlopt_flags = "-flarge-toc"
  **** run
*)

(* GPR#8506

   This isn't guaranteed to fail even without the fix from #8506, because
   the @ha relocation on the TOC entry for the exception handler's address
   might be zero, in which case the linker optimises the code sequence to one
   that will not fail.
*)

let () =
  try failwith "foo"
  with (Failure _) -> ()
