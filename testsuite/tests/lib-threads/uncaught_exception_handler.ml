(* TEST

flags = "-g"
ocamlrunparam += ",b=1"

* hassysthreads
include systhreads
** bytecode
** native

*)

(* Testing if uncaught exception handlers are behaving properly  *)

let () = Printexc.record_backtrace true

exception UncaughtHandlerExn
exception CallbackExn

let handler exn =
  let id = Thread.self () |> Thread.id in
  let msg = Printexc.to_string exn in
  Printf.eprintf "[thread %d] caught %s\n" id msg;
  Printexc.print_backtrace stderr;
  flush stderr;
  raise UncaughtHandlerExn

let fn () = Printexc.raise_with_backtrace
              CallbackExn
              (Printexc.get_raw_backtrace ())

let _ =
  let th = Thread.create fn () in
  Thread.join th;
  Thread.set_uncaught_exception_handler handler;
  let th = Thread.create fn () in
  Thread.join th
