(* TEST
 include unix;
 hasunix;
 not target-windows;
 { bytecode; }{ native; }
*)

let count () = Array.length (Sys.readdir "/dev/fd")
let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "203.0.113.1", 1)
let () =
  let before = count () in
  for _ = 1 to 100 do
    try Unix.establish_server (fun _ _ -> ()) addr with Unix.Unix_error _ -> ()
  done;
  Printf.printf "leaked fds: %d\n" (count () - before)
