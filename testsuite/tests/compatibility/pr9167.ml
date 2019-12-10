(* TEST
modules = "pr9167_stub.c"
*)

external retrieve_compare_unordered : unit -> bool = "retrieve_compare_unordered"

let bar = retrieve_compare_unordered ()
