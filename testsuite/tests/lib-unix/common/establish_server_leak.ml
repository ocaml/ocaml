(* TEST
 include unix;
 hasunix;
 not target-windows;
 { bytecode; }{ native; }
*)

let next_fd () =
  let fd = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  Unix.close fd;
  fd

let () =
  let busy = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind busy (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen busy 1;
  let addr = Unix.getsockname busy in
  let before = next_fd () in
  for _ = 1 to 10 do
    match Unix.establish_server (fun _ _ -> ()) addr with
    | () -> assert false
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) -> ()
  done;
  Printf.printf "leaked descriptors: %b\n" (next_fd () <> before);
  Unix.close busy
