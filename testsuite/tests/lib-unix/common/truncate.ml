(* TEST
include unix
files = "truncate.txt"
*)

let txt = "truncate.txt"

let size f =
  let ic = open_in_bin f in
  let n = in_channel_length ic in
  close_in ic;
  n

let () =
  Printf.printf "initial size: %d\n%!" (size txt);
  Unix.truncate txt (size txt - 2);
  Printf.printf "new size: %d\n%!" (size txt);
  Unix.truncate txt 0;
  Printf.printf "final size: %d\n%!" (size txt)
