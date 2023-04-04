(* TEST *)

let[@inline never][@local never] f n =
  let n = Int64.of_int n in
  let open Int64 in
  to_int (add n (of_int Int.min_int))

let _ =
  (* The test only works on architectures where Sys.int_size is 63,
     as it depends on the exact value of Int.min_int. *)
  if Sys.int_size <> 63 then
    Printf.printf "0x4000000000000001\n"
  else
    Printf.printf "0x%x\n%!" (f 1)
