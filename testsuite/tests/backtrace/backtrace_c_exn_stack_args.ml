(* TEST
 modules = "backtrace_c_exn_stack_args_.c";
 flags = "-g";
 ocamlrunparam += ",b=1";
*)

(* A C call whose arguments do not all fit in registers is made through a
   separate stub on several targets. Check that raising from such a call
   still produces a backtrace starting at the call site. *)
external raise_with_stack_args :
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  -> int -> unit
  = "backtrace_c_exn_stack_args_bytecode" "backtrace_c_exn_stack_args"

let[@inline never] f () = raise_with_stack_args 1 2 3 4 5 6 7 8 9 10 11 12

let () =
  try f () with
  | Not_found -> Printexc.print_backtrace stdout
