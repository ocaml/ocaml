(* TEST_BELOW *)

module Refs = struct
  (* standard atomics *)

  let standard_atomic_get (r : 'a Atomic.t) =
    Atomic.get r

  let standard_atomic_set (r : 'a Atomic.t) =
    Atomic.get r

  let standard_atomic_cas (r : 'a Atomic.t) oldv newv =
    Atomic.compare_and_set r oldv newv
end

module Fields = struct
  (* atomic record fields *)

  type 'a atomic = { filler : unit; mutable x : 'a [@atomic] }

  let get (r : 'a atomic) : 'a =
    r.x

  let explicit_get (r : 'a atomic) : 'a =
    Atomic.Loc.get [%atomic.loc r.x]

  let set (r : 'a atomic) v =
    r.x <- v

  let explicit_set (r : 'a atomic) v =
    Atomic.Loc.set [%atomic.loc r.x] v

  let cas (r : 'a atomic) oldv newv =
    Atomic.Loc.compare_and_set [%atomic.loc r.x] oldv newv
end

module Arrays = struct

  (* atomic arrays *)
  let array_get arr i =
    Atomic.Array.get arr i

  let unsafe_array_get arr i =
    Atomic.Array.unsafe_get arr i

  let array_set arr i v =
    Atomic.Array.set arr i v

  let unsafe_array_set arr i v =
    Atomic.Array.unsafe_set arr i v

  let array_cas arr i oldv newv =
    Atomic.Array.compare_and_set arr i oldv newv

  let unsafe_array_cas arr i oldv newv =
    Atomic.Array.unsafe_compare_and_set arr i oldv newv
end

(* TEST

  (* we restrict this test to a single configuration,
       amd64+linux not tsan no-flambda
     to avoid dealing with differences in cmm output across systems
     (the check is known to fail under MSCV, which uses a different
     symbol generator.)
   *)
   arch_amd64;
   linux;
   no-flambda; (* the output will be slightly different under Flambda *)
   not tsan; (* TSan modifies the generated code *)
   script = "sh ${test_source_directory}/check-reserved-bits.sh";
   script;

   setup-ocamlopt.byte-build-env;
   flags = "-c -dcmm -dno-locations -dno-unique-ids";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
*)
