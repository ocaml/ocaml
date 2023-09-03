(* Example where a side-effect invalidates static knowledge about
   a sub-value below the "toplevel" of the current matching context
   -- a sub-value of the current arguments of the pattern matrix.
 *)

type 'a myref = { mutable mut : 'a }
type u = {a: bool; b: (bool, int) Either.t myref }

let example_2 () =
  let input = { a = true; b = { mut = Either.Left true } } in
  match input with
  | {a = false; b = _} -> Result.Error 1
  | {a = _;     b = { mut = Either.Right _ }} -> Result.Error 2
  | {a = _;     b = _} when (input.b.mut <- Either.Right 3; false) -> Result.Error 3
  | {a = true;  b = { mut = Either.Left y }} -> Result.Ok y
