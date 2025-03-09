(* Example where a side-effect invalidates static knowledge about
   a sub-value at the "toplevel" of the current matching context
   -- on the current arguments of the pattern matrix.

   This example is adapted from the main example of #7241, showing
   that mutable state combined with context optimization could be
   unsound.

   The example of #7241 was adapted to return type-unsound result
   (on non-fixed systems) instead of segfaulting. Segfaults are
   painful to record and test reliably in a testsuite. *)

type u = {a: bool; mutable b: (bool, int) Either.t}

let example_1 () =
  let input = { a = true; b = Either.Left true } in
  match input with
  | {a = false; b = _} -> Result.Error 1
  | {a = _;     b = Either.Right _} -> Result.Error 2

  (* evil trick: mutate the scrutinee from a guard *)
  | {a = _;     b = _} when (input.b <- Either.Right 3; false) -> Result.Error 3

  (* At this point, field [b] has been mutated to hold a [Right]
     constructor, but the pattern-matching compiler has already
     checked read the field in the past and checked that the
     constructor was not [Right] -- otherwise the action [Error 2]
     would have been taken.

     The following behaviors would be reasonable on the input [f_input]:

     - read the field again, observe [Right], and fail with a match
       failure -- there is no clause left to match it.

     - reuse the previously read subvalue [Left true], and return [Ok true].

     For many years the OCaml compiler behaved incorrectly here: it
     would read the mutated value [Right 3], but assume from static
     context information that the head constructor is [Left]. and
     dereference its field without checking the constructor
     again. This returns the unsound result [Ok (3 : bool)]. *)
  | {a = true;  b = Either.Left y} -> Result.Ok y
;;
