(* TEST
   * bytecode
   * native
*)

external record_signal : int -> unit = "caml_sys_record_signal"

let _ =
  Sys.catch_break true ;
  try
    record_signal Sys.sigint ;
    (* poll *)
    let _ = Sys.opaque_identity (ref 1) in
    exit 1
  with
    Sys.Break -> exit 0
