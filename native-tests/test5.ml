(* input: 100000. Triggers heap overflow which invokes caml_call_gc, which
 * inturn causes GC to be invoked. *)

let rec mk_list length acc =
  if length < 1 then acc
  else mk_list (length-1) ((length-1)::acc)

let n =
  if Array.length Sys.argv != 2 then 100000
  else int_of_string @@ Sys.argv.(1)
let l = mk_list n []
let () = Printf.printf "%d\n" (List.hd l)
