(* TEST *)

type _ ty = Int : int ty | Bytes : bytes ty

let[@inline never] fill (type a) (p : a ty * a) =
  match p with
  | (Int, 0) -> ref "int"
  | (Int, _) -> ref "int"
  | (Bytes, b) ->
      Gc.minor ();
      let victim = ref "victim" in
      Bytes.fill b 0 (Bytes.length b) 'X';
      victim

let () =
  Gc.minor ();
  print_endline !(fill (Sys.opaque_identity (Bytes, Bytes.make 7 'b')))
