(* TEST
   bytecode;
*)

(* This is a regression test for a miscompilation in the
   pattern-matching compiler, which would only handle [x] values in
   the interval [min_int; 3], despite the match being correctly
   detected as partial.
*)
let[@warning "-partial-match"][@warning "-redundant-case"] f x y = match x, y with
  | _, true -> 0
  | (0 | 2), _ -> 1
  | 3, true -> 2
  | -1, _ -> 3

(* The switcher then generates an exhaustive-switch instruction which
   is passed below an input outside its input range, leading to
   a segfault. *)
let () =
  match f 4 false with
  | (n : int) -> ()
  | exception Match_failure _ -> ()
