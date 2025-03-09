(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
*)

let f n b =
  let arr = Array.make n 42 in
  if b then (arr, [| |]) else ([| |], arr)

let () =
  Printexc.record_backtrace true;
  match Sys.opaque_identity f (-1) true with
  | _ -> assert false
  | exception _ ->
    Printexc.print_backtrace stdout

(* TEST
 flags = "-g";
*)
