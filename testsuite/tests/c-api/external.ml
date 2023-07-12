(* TEST
 modules = "external_stubs.c";
{
   native;
 }
 *)

type data = A | B | C | D | E | F

external test_int : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (int [@untagged]) = "unavailable" "test" [@@noalloc]

external test_char : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (char [@untagged]) = "unavailable" "test" [@@noalloc]

external test_data : (int [@untagged])
                    -> (char [@untagged]) -> (data [@untagged])
                    -> (data [@untagged]) = "unavailable" "test" [@@noalloc]

external test_string : (string [@untagged]) -> (int [@untagged])
  = "unavailable" "tests" [@@noalloc]

external test_bytes : (bytes [@untagged]) -> (int [@untagged])
  = "unavailable" "tests" [@@noalloc]

open Bigarray

external test_bigarray : ((char, int8_unsigned_elt, c_layout) Array1.t [@untagged])
                     -> (int [@untagged]) = "unavailable" "tests" [@@noalloc]

external test_float_bigarray : ((float, float32_elt, c_layout) Array1.t [@untagged])
                     -> (int [@untagged]) -> (float [@unboxed])
  = "unavailable" "testf" [@@noalloc]

let _ = assert(test_int 1 '\001' B = 3)
let _ = assert(test_char 1 '\001' B = '\003')
let _ = assert(test_data 1 '\001' B = D)

let _ = assert(test_string "\001\001\002\003" = 7)
let _ = assert(test_bytes (Bytes.of_string "\001\001\002\003") = 7)

let tbl = Array1.init Bigarray.char c_layout 5 (fun i -> Char.chr (4 - i))

let _ = assert(test_bigarray tbl = 10)

let tblf = Array1.init Bigarray.float32 c_layout 5 (fun i -> float (4 - i))

let _ = assert(test_float_bigarray tblf (Array1.dim tblf) = 10.0)
