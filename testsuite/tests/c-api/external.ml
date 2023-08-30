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

let _ = assert(test_int 1 '\001' B = 3)
let _ = assert(test_char 1 '\001' B = '\003')
let _ = assert(test_data 1 '\001' B = D)
