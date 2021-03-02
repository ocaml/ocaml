(* TEST *)

let go () =
  Gc.full_major ();
  for i = 1 to 10_000 do
  let rec b =
    let x = (b, b) in
    (* Force the above allocation to be live across a GC,
       by allocating a large enough object that the allocations
       cannot be combined. *)
    let x = [| x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x; x; x; x; x; x; x; x; x; x; x; x; x; x; x; x;
               x |] in
    let _y = ref x in
    42 in
    ignore (Sys.opaque_identity b);
    ()
  done;
  ()

let _ =
  let _ = go () in
  print_endline "ok"
