let path_of_addr = function
  | Unix.ADDR_UNIX path -> path
  | _ -> assert false
;;

let test_sender ~client_socket ~server_socket ~server_addr ~client_addr =
  Printf.printf "%S" (path_of_addr client_addr);
  let byte = Bytes.make 1 't' in
  let sent_len = Unix.sendto client_socket byte 0 1 [] server_addr in
  assert (sent_len = 1);
  let buf = Bytes.make 1024 '\x00' in
  let (recv_len, sender) = Unix.recvfrom server_socket buf 0 1024 [] in

  Printf.printf " as %S: " (path_of_addr sender);
  assert (sender = client_addr);
  assert (Bytes.sub_string buf 0 recv_len = "t");
  print_endline "OK";;

let ensure_no_file path =
  try Unix.unlink path with Unix.Unix_error (ENOENT, _, _) -> ();;

let with_socket fn =
  let s = Unix.socket PF_UNIX SOCK_DGRAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close s) (fun () -> fn s)

let with_bound_socket path fn =
  with_socket (fun s ->
    let addr = Unix.ADDR_UNIX path in
    Unix.bind s addr;
    fn addr s
  )
