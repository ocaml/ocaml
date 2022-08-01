(* TEST
* hasunix
include unix
** bytecode
** native
*)

(* Checks that gethostbyaddr supports both IPv4 and IPv6 (see #11461) *)
let check a ty =
  let addr = Unix.inet_addr_of_string a in
  let host = Unix.gethostbyaddr addr in
  assert (host.Unix.h_addrtype = ty)

let () =
  check "127.0.0.1" Unix.PF_INET;
  check "::1" Unix.PF_INET6;
  print_endline "OK"
