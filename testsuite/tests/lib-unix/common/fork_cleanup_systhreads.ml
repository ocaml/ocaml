(* TEST
 include systhreads;
 hassysthreads;
 not-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* this test checks that the domain lock is properly reinitialized
   in the child process after fork.
   See: https://github.com/ocaml-multicore/ocaml-multicore/issues/471 *)

let () =
  let th = Thread.create (fun () -> Thread.delay 0.5) () in
  let fd = Unix.dup Unix.stdout in
  match Unix.fork () with
  | 0 ->
     Unix.close fd;
     print_endline "OK"
  | _ ->
     Unix.close fd;
     Thread.join th
