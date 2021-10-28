(* TEST
   * bytecode
*)

(* Test that evaluation order of constructor arguments is preserved.

   Depending on whether we evaluate the head argument or tail argument
   first, for a given call to `map`, there are two possible outputs:

        tl `n`                          \  printed in evaluation
        <prints from recursive calls>   /  of tl
        hd `n`                          >  printed in evaluation of hd

   and

        hd `n`                          > printed in evaluation of hd
        tl `n`                          \ printed in evaluation
        <prints from recursive calls>   / of tl

   With TMC, only the second version can happen, and this test ensures
   that the effects of [Format.printf "hd %d@." n; f x] are not moved
   inside the effectful [Format.printf "tl %d@." n; .] context.

   (Note that due to the left-to-right evaluation order, a non-TMC version
   would use the first version, and TMC is changing the evaluation order
   here -- this is allowed by the language specification, as long as
   each argument is fully evaluated before starting to evaluate another
   argument, which is what we are testing here)
*)
let [@tail_mod_cons] rec verbose_map n f xs =
  match xs with
  | [] -> Format.printf "nil %d@." n; []
  | x :: xs -> (Format.printf "hd %d@." n; f x) :: (Format.printf "tl %d@." n; verbose_map (n + 1)f xs)

let _ =
  assert (verbose_map 0 (fun x -> x + 1) [1; 2; 3] = [2; 3; 4])

(* Test that delayed constructors are properly restored inside non-TMC contexts *)
let[@tail_mod_cons] rec weird xs =
  () :: match xs with [] -> [] | x :: xs -> x :: weird xs

let _ =
  assert (weird [] = [()]);
  assert (weird [()] = [(); (); ()]);
  assert (weird [(); ()] = [(); (); (); (); ()]);
