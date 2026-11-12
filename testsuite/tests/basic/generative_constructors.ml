(* TEST *)

type 'a t =
  | Nil
  | Cons of 'a * 'a t [@generative]

let () =
  assert (Cons (0, Nil) != Cons (0, Nil))
