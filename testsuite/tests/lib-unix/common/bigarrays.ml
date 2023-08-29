(* TEST
include unix;
hasunix;
*)

let filename = "test.out"

let bigarray_of_string s =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (String.length s)
    (String.get s)

let string_of_bigarray buf =
  String.init (Bigarray.Array1.dim buf) (Bigarray.Array1.get buf)

let rec really_read f pos len =
  if len <= 0 then ()
  else
    let n = f pos len in
    really_read f (pos + n) (len - n)

let () =
  let fd = Unix.openfile filename [O_RDWR; O_CREAT; O_TRUNC] 0o644 in
  let str = ">hello, world<" in
  let buf = bigarray_of_string str in
  assert (Unix.write_bigarray fd buf 1 (String.length str - 2) = String.length str - 2);
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let buf = bigarray_of_string (String.map (fun _ -> 'X') str) in
  really_read (Unix.read_bigarray fd buf) 1 (String.length str - 2);
  Unix.close fd;
  print_endline (string_of_bigarray buf)
