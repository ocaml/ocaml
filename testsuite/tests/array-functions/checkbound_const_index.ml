(* TEST *)

(* Cmm_helpers.make_checkbound:
     | [Cop(Clsr, [a1; Cconst_int (n, _)], _); Cconst_int (m, _)]
       when (m lsl n) > n ->
         Cop(Ccheckbound, [a1; Cconst_int(m lsl n + 1 lsl n - 1, dbg)], dbg)
   [m lsl n] is computed with the compiler's 63-bit [int]; the guard does not
   detect every overflow. For a constant array index i = 2^54 + 1 (always out
   of bounds, since Max_wosize < 2^54) the tagged index is m = 2^55 + 3, and
   m lsl 9 wraps to 1536, so the bound check becomes [checkbound hdr 2047],
   which passes for any array of two or more elements. *)

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
