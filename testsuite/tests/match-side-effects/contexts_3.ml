(* Example where a side-effect invalidates static knowledge about
   a sub-value above the current matching context. *)

type 'a myref = { mutable mut : 'a }
type u = (bool * (bool, int) Either.t) myref

let example_3 () =
  let input = { mut = (true, Either.Left true) } in
  match input with
  | { mut = (false, _) } -> Result.Error 1
  | { mut = (_, Either.Right _) } -> Result.Error 2
  | { mut = (_, _) } when (input.mut <- (true, Either.Right 3); false) -> Result.Error 3
  | { mut = (true, Either.Left y) } -> Result.Ok y
