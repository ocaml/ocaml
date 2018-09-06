(* TEST
include unix
files = "truncate.txt ftruncate.txt"
*)

let txt = "truncate.txt"

let size s =
  (Unix.stat s).Unix.st_size

let () =
  Printf.printf "initial size: %d\n%!" (size txt);
  Unix.truncate txt (size txt - 2);
  Printf.printf "new size: %d\n%!" (size txt);
  Unix.truncate txt 0;
  Printf.printf "final size: %d\n%!" (size txt)

let ftxt = "ftruncate.txt"

let size fd =
  (Unix.fstat fd).Unix.st_size

let () =
  let fd = Unix.openfile ftxt [O_RDWR] 0 in
  Printf.printf "initial size: %d\n%!" (size fd);
  Unix.ftruncate fd (size fd - 3);
  Printf.printf "new size: %d\n%!" (size fd);
  Unix.ftruncate fd 0;
  Printf.printf "final size: %d\n%!" (size fd);
  Unix.close fd
