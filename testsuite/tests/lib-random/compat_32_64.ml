(* TEST *)

(* Check that [full_int] and [int_in_range] yield the same outputs on
 * every tested platform, regardless of the word size. *)

open Bigarray

let min_int31 = -0x4000_0000 (* = -2^30 *)
let max_int31 = 0x3FFF_FFFF (* = 2^30-1 *)

let _ =

  let a = Array1.of_array Int64 C_layout [| 1L; 2L; 3L; 4L |] in
  (* Violate abstraction of type Random.State.t to manipulate state directly *)
  let r = (Obj.magic a : Random.State.t) in

  (* [full_int], range that fits in 30 bits: *)
  for i = 0 to 49 do
    Printf.printf "%i\n" (Random.State.full_int r max_int31)
  done;
  print_newline ();

  (* [int_in_range], all-negative range whose length fits in 30 bits: *)
  for i = 0 to 49 do
    Printf.printf "%i\n"
      (Random.State.int_in_range r ~min:min_int31 ~max:(min_int31 + 996))
  done;
  print_newline ();

  (* [int_in_range], full 31-bit range: *)
  for i = 0 to 49 do
    Printf.printf "%i\n"
      (Random.State.int_in_range r ~min:min_int31 ~max:max_int31)
  done

let _ = exit 0
