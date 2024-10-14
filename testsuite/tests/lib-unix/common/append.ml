(* TEST
   include unix;
   hasunix;
   {
     bytecode;
   }{
     native;
   }
*)

let str = "Hello, OCaml!"

let append () =
  let fd =
    Unix.openfile "append.txt"
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ]
      0o644
  in
  let len = String.length str in
  let rec f = function
    | 0 -> ()
    | rem ->
        let n = Unix.write_substring fd str (len - rem) rem in
        f (rem - n)
  in
  f len;
  Unix.close fd

let () =
  append ();
  append ();
  let fd = Unix.openfile "append.txt" [ Unix.O_RDONLY ] 0o644 in
  let buf = Buffer.create 10 in
  let b = Bytes.create 10 in
  let rec f () =
    let n = Unix.read fd b 0 10 in
    Buffer.add_subbytes buf b 0 n;
    if n <> 0 then f ()
  in
  f ();
  Unix.close fd;
  assert (Buffer.contents buf = str ^ str)
