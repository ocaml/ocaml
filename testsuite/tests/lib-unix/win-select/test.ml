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
