(* TEST
   modules = "signals.c"
* bytecode
* native
* hassysthreads
include systhreads
** bytecode
** native

*)

(* This is a control test for the ability to explore all polling
   locations. Previous failures were loops when setting a signal
   handler from a signal handler inside caml_enter_blocking_section
   and when raising in bytecode. *)

external record_signal : int -> unit = "caml_sys_record_signal"
external process_uninterrupting : unit -> unit = "process_uninterrupting"
external blocking_section : unit -> unit = "blocking_section"

let n = 5000

exception Nth

let handler =
  let i = ref n in
  fun s ->
    decr i;
    if !i < 0 then raise Nth
    else (
      record_signal s;
      process_uninterrupting ();
    )

let _ =
  Sys.set_signal Sys.sigalrm (Signal_handle handler);
  record_signal Sys.sigalrm ;
  let () =
    (* caml_enter_blocking_section *)
    try
      blocking_section () ;
    with Nth -> failwith "enter/leave_blocking_section"
  in
  let () =
    (* raising exception (in bytecode) *)
    try
      try raise Exit with Exit -> ()
    with Nth -> failwith "raise (OCaml)"
  in
  ()
