(* TEST
   modules = "stubs.c"
*)

(* C version of ephetest.ml *)

let make_ra () = ref (ref 1) [@@inline never]
let make_rb () = ref (ref (ref 2)) [@@inline never]

let ra = make_ra ()
let rb = make_rb ()

external test1 : int ref ref -> int ref ref ref -> unit = "test1"
external test2 : int ref ref -> int ref ref ref -> unit = "test2"
external test3 : int ref ref -> int ref ref ref -> unit = "test3"
external test4 : int ref ref -> int ref ref ref -> unit = "test4"
external test5 : int ref ref -> int ref ref ref -> unit = "test5"
external test6 : int ref ref -> int ref ref ref -> unit = "test6"
external test7 : int ref ref -> int ref ref ref -> unit = "test7"
external test8 : int ref ref -> int ref ref ref -> unit = "test8"

let () =
  test1 ra rb;  test2 ra rb;  test3 ra rb;  test4 ra rb;  test5 ra rb;
  test6 ra rb;  test7 ra rb; test8 ra rb
