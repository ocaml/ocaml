(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints on C calls *)

(* Unsafe string, bytes and bigstring accesses *)
type bigstring =
  ( char,
    Stdlib.Bigarray.int8_unsigned_elt,
    Stdlib.Bigarray.c_layout )
  Stdlib.Bigarray.Array1.t

external string_get16 : string -> int -> int = "%caml_string_get16"

external string_unsafe_get16 : string -> int -> int = "%caml_string_get16u"

external bytes_unsafe_set32 : bytes -> int -> int32 -> unit
  = "%caml_bytes_set32u"

external bigstring_unsafe_get64 : bigstring -> int -> int64
  = "%caml_bigstring_get64u"

let string_get16 s i = string_get16 s i

let string_unsafe_get16 s i = string_unsafe_get16 s i

let bytes_unsafe_set32 b i v = bytes_unsafe_set32 b i v

let bigstring_unsafe_get64 b i = bigstring_unsafe_get64 b i

(* Bigarray accesses *)
external ba_get_1 : ('a, 'b, 'c) Stdlib.Bigarray.Array1.t -> int -> 'a
  = "%caml_ba_ref_1"

external ba_unsafe_set_2 :
  ('a, 'b, 'c) Stdlib.Bigarray.Array2.t -> int -> int -> 'a -> unit
  = "%caml_ba_unsafe_set_2"

let bigarray_float64_c
    (a :
      ( float,
        Stdlib.Bigarray.float64_elt,
        Stdlib.Bigarray.c_layout )
      Stdlib.Bigarray.Array1.t) i =
  ba_get_1 a i

let bigarray_int_fortran
    (a :
      ( int,
        Stdlib.Bigarray.int_elt,
        Stdlib.Bigarray.fortran_layout )
      Stdlib.Bigarray.Array2.t) i j v =
  ba_unsafe_set_2 a i j v

let bigarray_generic a i = ba_get_1 a i

(* Comparisons between boxed integers using polymorphic comparison
   functions *)
external int32_lt : int32 -> int32 -> bool = "%lessthan"

external int64_equal : int64 -> int64 -> bool = "%equal"

let int32_lt x y = int32_lt x y

let int64_equal x y = int64_equal x y

(* Comparison using a specialised function: no hint *)
external nativeint_compare : nativeint -> nativeint -> int = "%compare"

let nativeint_compare x y = nativeint_compare x y

(* Primitives whose native implementation differs from the bytecode one *)
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
  [@@unboxed] [@@noalloc]

external int_bits_of_float : float -> (int64[@unboxed])
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
  [@@noalloc]

let sqrt x = sqrt x

let int_bits_of_float x = int_bits_of_float x

(* Same implementation: no hint *)
external string_length : string -> int = "caml_ml_string_length"

let string_length s = string_length s
