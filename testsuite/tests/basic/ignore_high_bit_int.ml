(* TEST *)

(* Test the semantics of untagging and particularly what happens to
   the high bit during tagging/untagging. *)

let opaque_shift_by_int64 x n =
  x lsl Int64.(to_int (Sys.opaque_identity n))

let test_shift () =
  assert (42 = opaque_shift_by_int64 42 Int64.min_int);
  ()

let _ =
  test_shift ()

