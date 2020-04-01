(* TEST
include unix
* hasunix
** bytecode
** native
*)

let str = "Hello, OCaml!"
let txt = "truncate.txt"

let test file openfile stat truncate delta close =
  let () =
    let c = open_out_bin file in
    output_string c str;
    close_out c
  in
  let size file =
    (stat file).Unix.st_size
  in
  let file = openfile file in
  Printf.printf "initial size: %d\n%!" (size file);
  truncate file (size file - delta);
  Printf.printf "new size: %d\n%!" (size file);
  truncate file 0;
  Printf.printf "final size: %d\n%!" (size file);
  close file

let () =
  test "truncate.txt" (fun x -> x) Unix.stat Unix.truncate 2 ignore

let () =
  let open_it file = Unix.openfile file [O_RDWR] 0 in
  test "ftruncate.txt" open_it Unix.fstat Unix.ftruncate 3 Unix.close
