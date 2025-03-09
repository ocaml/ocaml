(* TEST *)

(* Regression test for #13448, see explanations in #13449.
   Another variant of the bug, with GADTs instead of flat float arrays *)
type _ t =
  | Int32 : int32 t
  | Float : float t

let constant = 42l

let[@inline always] default : type a . a t -> a = function
  | Float ->
     (* We want an expression that starts
        with box<float>(...). *)
     exp 0.
  | Int32 ->
     (* We want an expression that does not start
        with box<int32>(...). *)
     Sys.opaque_identity constant

let () =
  (* we use [opaque_identity] so that [default gadt] is not
     reduced at compile-time. *)
  let gadt = Sys.opaque_identity Int32 in
  let n = default gadt in
  assert (n = constant)
