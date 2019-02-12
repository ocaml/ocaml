(* TEST
*)

type t =
| A of string * float
| B of string * float

let () =
  assert (Obj.dup (Obj.repr (A ("hello", 10.))) = Obj.repr (A ("hello", 10.)));
  assert (Obj.with_tag 1 (Obj.repr (A ("hello", 10.))) = Obj.repr (B ("hello", 10.)));
  print_endline "ok"
