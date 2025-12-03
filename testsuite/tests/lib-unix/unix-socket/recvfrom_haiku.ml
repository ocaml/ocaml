(* TEST
 include unix;
 modules = "recvfrom.ml";
 script;
 {
   bytecode;
 }{
   native;
 }
*)
open Recvfrom

let () =
  let server_path = "ocaml-test-socket-haiku" in
  ensure_no_file server_path;
  at_exit (fun () -> ensure_no_file server_path);
  with_bound_socket server_path (fun server_addr server_socket ->
    (* abstract socket *)
    with_bound_socket "\x00123fe" (fun client_addr client_socket ->
      test_sender ~client_socket ~server_socket ~server_addr ~client_addr
    );
  )
