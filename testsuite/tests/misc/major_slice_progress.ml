(* TEST *)

(* Repeated calls to [Gc.major_slice] must complete major cycles even in
   the absence of allocation: forced slices spend their budget on the
   post-sweep idle phase, which is otherwise driven by allocation. *)

let () =
  let before = (Gc.quick_stat ()).major_collections in
  (* This loop does not allocate. *)
  for _ = 1 to 2_000 do
    ignore (Gc.major_slice 10_000 : int)
  done;
  let after = (Gc.quick_stat ()).major_collections in
  if after - before >= 3
  then print_endline "ok"
  else Printf.printf "error: only %d major cycles completed\n" (after - before)
