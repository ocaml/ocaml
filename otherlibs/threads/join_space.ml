(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*      Luc Maranget, projet Moscova                                   *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Unix

let creation_time = gettimeofday ();;
let localname = gethostname ();;
let first_addr entry = entry.h_addr_list.(0);;

let localname = (* try to bind a socket to a INET port, if not possible,
use loopback (127.0.0.1) instead... *)
  let s = socket PF_INET SOCK_STREAM 0 in
  setsockopt s SO_REUSEADDR true;  
  let localaddr = 
    try
      first_addr (gethostbyname localname) 
    with
    | _ -> inet_addr_of_string "127.0.0.1"
    in
  let localname = 
    try
      bind s (ADDR_INET (localaddr, 0));
      localname
    with
      _ -> 
        Printf.printf "WARNING: Using loopback address (127.0.0.1). you will not be able to communicate with other machines. Verify your network connection if this is not normal.";
        print_newline ();
        let localaddr = inet_addr_of_string "127.0.0.1" in
        bind s (ADDR_INET (localaddr, 0));
        "localhost"
  in
  close s;
  localname

let localaddr = 
  try
    first_addr (gethostbyname localname)
  with
  | _ -> inet_addr_of_string "127.0.0.1"
;;

let create_port port =
  handle_unix_error
    (fun () ->
      let s = socket PF_INET SOCK_STREAM 0 in
      let _ =
        setsockopt s SO_REUSEADDR true;
        bind s (ADDR_INET (localaddr, port));
        listen s 5;
        ()
      in
      let saddr = getsockname s in
      match saddr with
        ADDR_INET (_, port) -> port, s
      | _ -> failwith "not ADDR_INET socket") ()

(* Create site socket *)
let local_port, local_socket = create_port 0
;;

(* And compute global site identifier *)
let local_id = localaddr, local_port, creation_time
;;

(* Allocate a new unique ident for local site *)
let new_uid =
  let uid_counter = ref 0 in
  (fun () -> incr uid_counter; !uid_counter)
;;
