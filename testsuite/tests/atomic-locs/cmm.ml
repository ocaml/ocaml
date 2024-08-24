(* TEST_BELOW *)

(* standard atomics *)
let standard_atomic_get (r : 'a Atomic.t) =
  Atomic.get r

let standard_atomic_cas (r : 'a Atomic.t) oldv newv =
  Atomic.compare_and_set r oldv newv

(* TEST

  (* we restrict this test to a single configuration,
       amd64+linux no-tsan no-flambda
     to avoid dealing with differences in cmm output across systems
     (the check is known to fail under MSCV, which uses a different
     symbol generator.)
   *)
   arch_amd64;
   linux;
   no-flambda; (* the output will be slightly different under Flambda *)
   no-tsan; (* TSan modifies the generated code *)

   setup-ocamlopt.byte-build-env;
   flags = "-c -dcmm -dno-locations -dno-unique-ids";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
*)
