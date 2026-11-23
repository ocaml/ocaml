(* TEST
   native;
*)

(* There is a simplification in [Cmm_helpers.make_checkbound] that combines
an lsr in the bound with the index. This combination using could possibly
overflow if it is not checked *)

let[@inline never] get_int (a : int array) = a.(0x40_0000_0000_0001)

let[@inline never] set_int (a : int array) v = a.(0x40_0000_0000_0001) <- v

let[@inline never] get_float (a : float array) = a.(0x40_0000_0000_0001)

let[@inline never] get_generic (a : 'a array) = a.(0x40_0000_0000_0001)

let check name f =
  match f () with
  | () -> Printf.printf "%s: FAIL (no exception)\n%!" name
  | exception Invalid_argument _ -> Printf.printf "%s: ok\n%!" name

let () =
  check "int array get" (fun () -> ignore (get_int (Array.make 4 0)));
  check "int array set" (fun () -> set_int (Array.make 4 0) 1);
  check "float array get" (fun () -> ignore (get_float (Array.make 4 0.)));
  check "generic array get" (fun () -> ignore (get_generic (Array.make 4 "")))
