(* TEST
include unix
modules = "recvfrom.ml"
* hasunix
** not-windows
*** bytecode
*** native
*)
open Recvfrom

let () =
  let server_path = "ocaml-test-socket-unix" in
  ensure_no_file server_path;
  at_exit (fun () -> ensure_no_file server_path);
  with_bound_socket server_path (fun server_addr server_socket ->
    (* path socket, just reuse server addr *)
    test_sender ~client_socket:server_socket ~server_socket ~server_addr ~client_addr:server_addr;

    (* unnamed socket *)
    with_socket (fun client_socket ->
      (* unbound socket should be treated as empty path *)
      test_sender ~client_socket ~server_socket ~server_addr ~client_addr:(ADDR_UNIX "")
    )
  )
