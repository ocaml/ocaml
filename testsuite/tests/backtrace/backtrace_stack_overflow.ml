(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

(* The backtrace of Stack_overflow raised by caml_call_realloc_stack *)

let rec f n = if n = 0 then 0 else 1 + f (Sys.opaque_identity (n - 1))

let () =
  try ignore (f max_int) with Stack_overflow ->
    let lines =
      String.split_on_char '\n' (Printexc.get_backtrace ()) in
    List.iteri (fun i l -> if i < 2 then print_endline l) lines

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1,l=100000";
 not tsan;
 native;
*)
