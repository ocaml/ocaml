(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Paris                  *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Printf

(* Regression test for PR#5325: simultaneous read and write on socket
   in Windows. *)

(* Scenario:
     - thread [server] implements a simple 'echo' server on a socket
     - thread [reader] reads from a socket connected to the echo server
       and copies to standard output
     - main program executes [writer], which writes to the same socket
       (the one connected to the echo server)
     - thread [timeout] causes a failure if nothing happens in 10 seconds.
*)

let serve_connection s =
  let buf = String.make 1024 '>' in
  let n = Unix.read s buf 2 (String.length buf - 2) in
  ignore (Unix.write s buf 0 (n + 2));
  Unix.close s

let server sock =
  while true do
    let (s, _) = Unix.accept sock in
    ignore(Thread.create serve_connection s)
  done

let timeout () =
  Thread.delay 10.0;
  printf "Time out, exiting...\n%!";
  exit 2

let reader s =
  let buf = String.make 1024 ' ' in
  let n = Unix.read s buf 0 (String.length buf) in
  print_string (String.sub buf 0 n); flush stdout

let writer s msg =
  ignore (Unix.write s msg 0 (String.length msg));
  Unix.shutdown s Unix.SHUTDOWN_SEND

let _ =
  let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 9876) in
  let serv =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt serv Unix.SO_REUSEADDR true;
  Unix.bind serv addr;
  Unix.listen serv 5;
  ignore (Thread.create server serv);
  ignore (Thread.create timeout ());
  Thread.delay 0.5;
  let client =
    Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect client addr;
  let rd = Thread.create reader client in
  Thread.delay 0.5;
  writer client "Client data\n";
  Thread.join rd
