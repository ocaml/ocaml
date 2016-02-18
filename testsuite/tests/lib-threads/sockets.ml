(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

(* Threads and sockets *)

let serve_connection s =
  let buf = String.make 1024 '>' in
  let n = Unix.read s buf 2 (String.length buf - 2) in
  Thread.delay 1.0;
  ignore (Unix.write s buf 0 (n + 2));
  Unix.close s

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let client (addr, msg) =
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect sock addr;
  let buf = String.make 1024 ' ' in
  ignore(Unix.write sock msg 0 (String.length msg));
  let n = Unix.read sock buf 0 (String.length buf) in
  print_string (String.sub buf 0 n); flush stdout

let _ =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 9876) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock addr;
  Unix.listen sock 5;
  ignore (Thread.create server sock);
  ignore (Thread.create client (addr, "Client #1\n"));
  Thread.delay 0.5;
  client (addr, "Client #2\n")
