(* TEST

* frame_pointers
** native
readonly_files = "fp_backtrace.c c_call_.c"
all_modules = "${readonly_files} c_call.ml"

*)

external fp_backtrace : unit -> unit = "fp_backtrace"
external fp_backtrace_no_alloc : unit -> unit = "fp_backtrace" [@@noalloc]
external fp_backtrace_many_args : int -> int -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> unit =
  "fp_backtrace_many_args_argv" "fp_backtrace_many_args"

let[@inline never] f () =
  (* Check backtrace through caml_c_call_stack_args *)
  fp_backtrace_many_args 1 2 3 4 5 6 7 8 9 10 11;
  (* Check backtrace through caml_c_call.
   * Also check that caml_c_call_stack_args correclty restores rbp register *)
  fp_backtrace ();
  (* Check caml_c_call correclty restores rbp register *)
  fp_backtrace_no_alloc ();
  42

let () = ignore (f ())
