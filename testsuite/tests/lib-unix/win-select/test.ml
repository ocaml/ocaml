(* TEST
include unix;
hasunix;
target-windows;
{ bytecode; }{ native; }
*)

open Unix

let () =
  let s1 = socket PF_INET SOCK_STREAM 0 in
  bind s1 (ADDR_INET (inet_addr_any, 0));

  let pr, pw = pipe () in
  let _ = write pw (Bytes.of_string "x") 0 1 in

  let s2 = socket PF_INET SOCK_STREAM 0 in
  bind s2 (ADDR_INET (inet_addr_any, 0));

  let fds = [s1; pr; s2] in
  let (ready_read, _, _) = select fds [] [] 0.5 in

  if not (List.mem pr ready_read) then
    Printf.printf "BUG REPRODUCED: Pipe handle was LOST due to list truncation!\n"
  else
    Printf.printf "SUCCESS: Pipe handle is still tracked.\n";

  close pr; close pw;
  close s1; close s2

let () =
  let server = socket PF_INET SOCK_STREAM 0 in
  bind server (ADDR_INET (inet_addr_loopback, 0));
  listen server 1;
  let port =
    match getsockname server with
    | ADDR_INET (_, port) -> port
    | _ -> assert false
  in
  let client = socket PF_INET SOCK_STREAM 0 in
  connect client (ADDR_INET (inet_addr_loopback, port));
  let accepted, _ = accept server in
  let pr, pw = pipe () in

  begin match select [pr; client] [client] [] 0.5 with
  | _, [fd], _ when fd == client ->
      Printf.printf "SUCCESS: Socket uses the right per-list result.\n"
  | _ ->
      Printf.printf "BUG REPRODUCED: Wrong write result.\n"
  | exception Failure s when s = "select.c: original file handle not found" ->
      Printf.printf "BUG REPRODUCED: Socket uses an index from another list.\n"
  end;

  close pr; close pw;
  close accepted; close client; close server

let () =
  let server = socket PF_INET SOCK_STREAM 0 in
  bind server (ADDR_INET (inet_addr_loopback, 0));
  listen server 1;
  let port =
    match getsockname server with
    | ADDR_INET (_, port) -> port
    | _ -> assert false
  in
  let client = socket PF_INET SOCK_STREAM 0 in
  connect client (ADDR_INET (inet_addr_loopback, port));
  let accepted, _ = accept server in
  let decoy = socket PF_INET SOCK_STREAM 0 in
  bind decoy (ADDR_INET (inet_addr_loopback, 0));
  listen decoy 1;
  let pr, pw = pipe () in
  (* [client]'s query incorrectly retains index 1 from [readfds]. Put a
     non-writable listening socket at index 1 in [writefds], so the stale index
     returns the wrong descriptor instead of being out of bounds. *)
  begin match select [pr; client] [client; decoy] [] 0.5 with
  | _, [fd], _ when fd == client ->
      Printf.printf "SUCCESS: Socket is not confused with another descriptor.\n"
  | _, [fd], _ when fd == decoy ->
      Printf.printf "BUG REPRODUCED: Wrong write descriptor returned.\n"
  | _ ->
      Printf.printf "BUG REPRODUCED: Unexpected write result.\n"
  end;
  close pr; close pw;
  close decoy;
  close accepted; close client; close server

let () =
  let pr, pw = pipe () in
  let path = Filename.temp_file "win-select" ".tmp" in
  let file = openfile path [O_RDONLY] 0 in

  begin match select [pr; pr; file] [] [] 0.5 with
  | [fd], _, _ when fd == file ->
      Printf.printf "SUCCESS: Descriptor after a duplicate is mapped correctly.\n"
  | _ ->
      Printf.printf "BUG REPRODUCED: Duplicate changed a result index.\n"
  end;

  close file; unlink path;
  close pr; close pw
