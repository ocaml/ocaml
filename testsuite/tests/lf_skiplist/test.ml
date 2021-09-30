(* TEST
   modules = "stubs.c"
*)

external test_skiplist_serial : unit -> unit = "test_skiplist_serial"

let () = test_skiplist_serial ()