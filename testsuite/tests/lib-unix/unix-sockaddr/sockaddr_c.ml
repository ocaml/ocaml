(* TEST
 modules = "sockaddr_c_aux.c";
 include unix;
 hasunix;
 native;
*)
external stubs : Unix.sockaddr -> Unix.sockaddr = "stubs"

let () =
  let addr = Unix.(ADDR_INET (inet6_addr_any, 0)) in
  let addr' = stubs addr in
  assert (addr = addr')
