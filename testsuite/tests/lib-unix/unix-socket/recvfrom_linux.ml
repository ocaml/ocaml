(* TEST
include unix
modules = "recvfrom.ml"
script = "sh ${test_source_directory}/is-linux.sh"
* hasunix
** script
*** bytecode
*** native
*)
open Recvfrom

let () =
  let server_path = "ocaml-test-socket-linux" in
  ensure_no_file server_path;
  at_exit (fun () -> ensure_no_file server_path);
  with_bound_socket server_path (fun server_addr server_socket ->
    (* abstract socket *)
    with_bound_socket "\x00ocaml-abstract-socket" (fun client_addr client_socket ->
      test_sender ~client_socket ~server_socket ~server_addr ~client_addr
    );
  )
