(* TEST
 include unix;
 hasunix;
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* On Windows, Unix.select falls back to a worker thread emulation as soon as
   the descriptor set is not made exclusively of sockets. That path registers
   every socket with WSAEventSelect and undoes it at the end of the call. An
   error used to skip the whole cleanup, leaving the sockets registered and in
   non-blocking mode, and leaking one event handle per socket.

   ioctlsocket(FIONBIO) fails with WSAEINVAL while a WSAEventSelect
   registration is active, so Unix.clear_nonblock detects a skipped cleanup. *)

let () =
  let listening = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind listening (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen listening 1;
  let client = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect client (Unix.getsockname listening);
  let server, _ = Unix.accept listening in

  (* The pipe is what forces select onto the emulation path. *)
  let pipe_read, pipe_write = Unix.pipe () in
  (* A descriptor closed behind select's back, as happens when a server closes
     a client socket from another thread. WSAEventSelect then fails on it, and
     it must not prevent the other sockets from being cleaned up. *)
  let closed = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.close closed;

  begin match Unix.select [server; closed; pipe_read] [] [] 0.0 with
  | _ -> print_string "UNEXPECTED: select did not fail on a closed socket\n"
  | exception Unix.Unix_error _ ->
    begin match Unix.clear_nonblock server with
    | () -> print_string "OK\n"
    | exception Unix.Unix_error (Unix.EINVAL, _, _) ->
      print_string "FAIL: socket still registered with WSAEventSelect\n"
    end
  end;

  Unix.close pipe_read;
  Unix.close pipe_write;
  Unix.close server;
  Unix.close client;
  Unix.close listening
