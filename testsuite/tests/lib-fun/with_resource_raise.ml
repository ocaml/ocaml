(* TEST
   * bytecode
   * native

*)

type pass = Pass | Fail

let report b =
  print_endline (if b == Pass then "Passed" else "Failed")

let () =
  Printexc.set_uncaught_exception_handler (fun _ bt ->
    print_endline "Uncaught exception in destructor";
    Printexc.print_raw_backtrace stderr bt ;
    report Fail ;
    exit 1
  )

let poll () = ignore (Sys.opaque_identity (ref 0))

external record_signal : int -> unit = "caml_sys_record_signal"

(* Basic resource-safety *)
let control () =
  print_endline "Control:" ;
  match
    record_signal Sys.sigint ;
    poll ()
  with
  | () -> report Fail
  | exception Sys.Break -> report Pass

let test2 num =
  Printf.printf "Test (num = %n):\n" num ;
  let flag = ref 0 in
  let do_signal n = if n = num then record_signal Sys.sigint in
  (try
     do_signal 0 ;
     Fun.with_resource
       ~acquire:(fun () -> do_signal 1 ; incr flag ; poll () ; do_signal 2) ()
       ~scope:(fun () -> do_signal 3 ; poll () ; do_signal 4)
       ~release:(fun () -> do_signal 5 ; poll () ; decr flag ; do_signal 6) ;
     poll () ;
   with Sys.Break -> ()
  ) ;
  report (if !flag = 0 then Pass else Fail)

let _ =
  Sys.catch_break true ;
  control () ;
  for i = 0 to 6 do
    test2 i
  done
