(* TEST
*)

(* Check the numbers drawn from a known state against the numbers
   obtained from the reference Java implementation. *)

open Bigarray

let _ =
  let a = Array1.of_array Int64 C_layout [| 1L; 2L; 3L; 4L |] in
  (* Violate abstraction of type Random.State.t to manipulate state directly *)
  let r = (Obj.magic a : Random.State.t) in
  for i = 0 to 49 do
    Printf.printf "%Ld\n" (Random.State.bits64 r)
  done

let _ = exit 0
