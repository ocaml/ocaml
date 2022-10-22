(* TEST
* hasunix
include unix
** bytecode
** native
*)

(* Checks that gethostbyaddr supports both IPv4 and IPv6 (see #11461) *)
let check a ty =
  match Unix.inet_addr_of_string a with
  | exception (Failure _) ->
      (* IPv6 addresses not supported on this platform, just ignore *)
      ()
  | addr ->
      match Unix.gethostbyaddr addr with
      | exception Not_found ->
          (* Name resolver badly configured? (observed on OmniOS).
             Just ignore *)
          ()
      | host ->
          assert (host.Unix.h_addrtype = ty)

let () =
  check "127.0.0.1" Unix.PF_INET;
  check "::1" Unix.PF_INET6;
  print_endline "OK"
