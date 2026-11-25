(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

let check (name, port) =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect ~finally:(fun () -> Unix.close s) (fun () ->
    match Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, port)) with
    | () -> Printf.printf "%s: ok\n" name
    | exception Invalid_argument _ ->
        Printf.printf "%s: Invalid_argument\n" name)

let () =
  List.iter check
    ["0", 0; "65535", 65535; "-1", -1; "65536", 65536; "70000", 70000;
     "max_int", max_int; "min_int", min_int]
