open Unix

let path_of_addr = function
  | ADDR_UNIX path -> path
  | _ -> assert false
;;

let test_sender ~client_socket ~server_socket ~server_addr ~client_addr =
  Printf.printf "%S" (path_of_addr client_addr);
  let byte = Bytes.make 1 't' in

  (* Printf.printf "[sending]"; (* DEBUG *) *)

  let sent_len = sendto client_socket byte 0 1 [] server_addr in
  assert (sent_len = 1);
  let buf = Bytes.make 1024 '\x00' in

  let sender =
    (* try *)
      let (recv_len, sender) = recvfrom server_socket buf 0 1024 [] in
      assert (Bytes.sub_string buf 0 recv_len = "t");
      assert (sender = client_addr);
      sender
    (* with e -> ADDR_UNIX (Printexc.to_string e) *)
  in

  Printf.printf " as %S: " (path_of_addr sender);

  (* Printf.printf "[receiving]"; (* DEBUG *) *)

  print_endline "OK";;

let ensure_no_file path =
  try unlink path with Unix_error (ENOENT, _, _) -> ();;

let with_socket fn =
  let s = socket PF_UNIX SOCK_DGRAM 0 in
  Fun.protect ~finally:(fun () -> close s) (fun () -> fn s)

let with_bound_socket path fn =
  with_socket (fun s ->
    let addr = ADDR_UNIX path in
    bind s addr;
    fn addr s
  )
