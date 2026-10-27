(* TEST
 flags = "-S";
 arch_arm64;
 native;
*)

let[@inline never] compare_ints (x : int) (y : int) =
  compare x y

let check x y expected =
  assert (compare_ints (Sys.opaque_identity x) (Sys.opaque_identity y) = expected)

let () =
  check Int.min_int Int.max_int (-1);
  check (-1) (-1) 0;
  check Int.max_int Int.min_int 1
