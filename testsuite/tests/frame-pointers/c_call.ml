(* TEST
 frame_pointers;
 readonly_files = "fp_backtrace.c c_call_.c";
 all_modules = "${readonly_files} c_call.ml";
 native;
*)

external fp_backtrace : string -> unit = "fp_backtrace"
external fp_backtrace_no_alloc : string -> unit = "fp_backtrace" [@@noalloc]
external fp_backtrace_many_args : string -> int -> int -> int -> int -> int
  -> int -> int -> int -> int -> int -> int -> unit =
  "fp_backtrace_many_args_argv" "fp_backtrace_many_args"

let[@inline never] f () =
  (* Check backtrace through caml_c_call_stack_args *)
  fp_backtrace_many_args Sys.argv.(0) 1 2 3 4 5 6 7 8 9 10 11;
  (* Check backtrace through caml_c_call.
   * Also check that caml_c_call_stack_args correctly restores rbp register *)
  fp_backtrace Sys.argv.(0);
  (* Check caml_c_call correctly restores rbp register *)
  fp_backtrace_no_alloc Sys.argv.(0);
  42

let () = ignore (f ())
