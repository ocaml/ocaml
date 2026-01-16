(* TEST
 modules = "user_data_regression_stub.c";
 flags = "-g";
 ocamlrunparam += ",s=100";
 { bytecode; }
 { native; }
*)

external request_minor_gcs : unit -> unit = "request_minor_gcs"

let deep f =
  let rec loop n = if n = 0 then (f (); 0) else 1 + loop (n-1) in
  ignore (loop (Random.int 100))

let () =
  let alloc_minor (info : Gc.Memprof.allocation) =
    ignore (Printexc.raw_backtrace_to_string info.callstack);
    None
  in
  Gc.Memprof.(start ~sampling_rate:1. {null_tracker with alloc_minor}) |> ignore;
  for i = 1 to 1_000 do
    deep (fun () ->
      request_minor_gcs ();
      ignore (Sys.opaque_identity (ref 42)));
  done;
  print_endline "ok"
