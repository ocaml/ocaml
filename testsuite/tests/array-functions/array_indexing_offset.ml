(* TEST *)

(* Cmm_helpers.array_indexing, case
     | Cop(Caddi, [c; Cconst_int (n, _)], _) ->
         Cop(add, [Cop(add, [ptr; lsl_const c (log2size - 1) dbg], dbg);
                   Cconst_int((n-1) lsl (log2size - 1), dbg)], dbg)
   The constant [(n-1) lsl (log2size - 1)] is computed with the compiler's
   63-bit [int] and wraps when [n] is large, although the index [c + n] is
   small. The address is then off by 2^63.

   Here the index is [i - 2^60] with [i = 2^60 + 1]: the tagged constant is
   n = -2^61, and (n-1) lsl 2 = -2^63 - 4 does not fit in an [int]. *)

let k = 0x1000_0000_0000_0000 (* 2^60 *)

let i = k + 1

let[@inline never] get_int (a : int array) i = Array.unsafe_get a (i - k)

let[@inline never] get_float (a : float array) i = Array.unsafe_get a (i - k)

let[@inline never] set_int (a : int array) i v = Array.unsafe_set a (i - k) v

let[@inline never] get_float64
    (b : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t) i =
  Bigarray.Array1.unsafe_get b (i - k)

let check name ok = Printf.printf "%s: %s\n%!" name (if ok then "ok" else "FAIL")

let () =
  let ints = [| 10; 11; 12 |] in
  check "int array unsafe_get" (get_int ints i = 11);
  let floats = [| 1.0; 2.0; 3.0 |] in
  check "float array unsafe_get" (get_float floats i = 2.0);
  set_int ints i 42;
  check "int array unsafe_set" (ints.(1) = 42);
  let ba = Bigarray.(Array1.of_array float64 c_layout [| 1.0; 2.0; 3.0 |]) in
  check "float64 bigarray unsafe_get" (get_float64 ba i = 2.0)
