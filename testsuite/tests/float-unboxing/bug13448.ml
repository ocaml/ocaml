(* TEST *)

(* Regression test for #13448, see explanations in #13449.
   This minimized test was proposed by Nicolas Ojeda Bar.
*)

external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
let uget =
  (* This intermediate definition avoids primitive specialization in
     lambda/translprim at the call site below (so that the access
     remain at kind Pgenval in the Lambda representation), but does
     not prevent inlining during the Closure pass. *)
  unsafe_get

let () =
  let int32 = 123456l in
  let arr = [| int32 |] in
  let n = uget arr 0 in
  assert (n = int32)
