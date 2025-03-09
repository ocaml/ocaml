(* TEST *)

open Bigarray

(* Check that type-specialized accesses produce the same results
   as generic accesses *)

let generic (a: ('a, 'b, 'c) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_float16 (a: (float, float16_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_float32 (a: (float, float32_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_float64 (a: (float, float64_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int8s (a: (int, int8_signed_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int8u (a: (int, int8_unsigned_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int16s (a: (int, int16_signed_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int16u (a: (int, int16_unsigned_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int32 (a: (int32, int32_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int64 (a: (int64, int64_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_int (a: (int, int_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_nativeint (a: (nativeint, nativeint_elt, c_layout) Array1.t)
                      v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_complex32 (a: (Complex.t, complex32_elt, c_layout) Array1.t)
                      v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_complex64 (a: (Complex.t, complex64_elt, c_layout) Array1.t)
                      v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let special_char (a: (char, int8_unsigned_elt, c_layout) Array1.t) v0 v1 v2 =
  a.{0} <- v0; a.{1} <- v1; a.{2} <- v2;
  (a.{0}, a.{1}, a.{2})

let test kind special v0 v1 v2 =
  let a = Array1.create kind c_layout 3 in
  let s = special a v0 v1 v2 in
  let g = generic a v0 v1 v2 in
  assert (s = g)

let _ =
  test float16 special_float16 1.0 (-2.0) Float.pi;
  test float32 special_float32 1.0 (-2.0) Float.pi;
  test float64 special_float64 1.0 (-2.0) Float.pi;
  test int8_signed special_int8s 123 (-456) 0xFF00FF;
  test int8_unsigned special_int8u 123 (-456) 0xFF00FF;
  test int16_signed special_int16s 123 (-456) 0xFF00FF;
  test int16_unsigned special_int16u 123 (-456) 0xFF00FF;
  test int32 special_int32 123l (-456l) (0x22334455l);
  test int64 special_int64 123L (-456L) (0x2233445566778899L);
  test int special_int 123 (-456) 0xFF00FF;
  test nativeint special_nativeint 123n (-456n) (0x22334455n);
  test complex32 special_complex32 Complex.zero Complex.one Complex.i;
  test complex64 special_complex64 Complex.zero Complex.one Complex.i;
  test char special_char 'A' '-' 'Z'
