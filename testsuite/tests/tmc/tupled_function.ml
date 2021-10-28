(* TEST
   * bytecode
   * native
*)
type 'a t =
  | N of 'a
  | C of 'a t * 'a t

(* This function is recognized as 'tupled' by the backend; it is
   a regression-test to check that our TMC transformation works as
   expected for tupled (rather than curried) functions.

   Note: it is important to test the 'native' compiler here, as the
   bytecode does not perform the same arity-raising optimizations. *)
let[@tail_mod_cons] rec map (f, l) =
  match l with
  | N v -> N (f v)
  | C (a, b) ->
      C (map (f, a), (map [@tailcall]) (f, b))

let v = C (C (N 1, N 2), N 3)

let v' =
  let arg = (succ, v) in
  map arg

let () =
  assert (v' = C (C (N 2, N 3), N 4))
