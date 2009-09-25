(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(* Configuration *)

type fork_args =
| No_argument
| Same_arguments of string array
| Argument_generator of (unit -> string array)

let prepend s a =
  let len = Array.length a in
  Array.init
    (succ len)
    (function
      | 0 -> String.copy s
      | i -> a.(pred i))

let get_args = function
  | No_argument -> [| |]
  | Same_arguments a -> a
  | Argument_generator f -> f ()

let rec do_forks name args = function
  | 0 -> []
  | n ->
      match Unix.fork () with
      | 0 ->
          let args = prepend name (get_args args) in
          (try Unix.execv name args with _ -> exit 127)
      | pid -> pid :: (do_forks name args (pred n))

type configuration = {
    mutable host : string;
    mutable port : int;
    mutable clients : int;
    mutable client_name : string;
    mutable fork_args : fork_args;
    mutable magic_id : string;
    mutable magic_value : string;
}

let default_host = "localhost"

let default_port = 12345

let default_clients = 0

let default_client_name = String.copy Sys.argv.(0)

let default_fork_args =
  let len = Array.length Sys.argv in
  Same_arguments (Array.init (pred len) (fun i -> String.copy Sys.argv.(succ i)))

let default_magic_id = "magic-number"

let default_magic_value = "magic-value"

let default_configuration () = {
  host = String.copy default_host;
  port = default_port;
  clients = default_clients;
  client_name = String.copy default_client_name;
  fork_args = default_fork_args;
  magic_id = String.copy default_magic_id;
  magic_value = String.copy default_magic_value;
}

let split_addr s =
  try
    let idx = String.index s ':' in
    let host = String.sub s 0 idx in
    let port = String.sub s (succ idx) (String.length s - idx - 1) in
    (if host = "" then String.copy default_host else host),
    (try int_of_string port with _ -> raise (Arg.Bad ("invalid port: " ^ port)))
  with
  | Not_found -> String.copy s, default_port

let make_configuration ?(key_prefix="-join") () =
  let cfg = default_configuration () in
  cfg, [ key_prefix ^ "-host",
         Arg.String
           (fun s ->
             let h, p = split_addr s in
             cfg.host <- h;
             cfg.port <- p),
         "<name:port>  Set host name and port" ;

         key_prefix ^ "-clients",
         Arg.Int (fun i -> cfg.clients <- i),
         "<n>  Set number of clients to launch";

         key_prefix ^ "-client-name",
         Arg.String (fun s -> cfg.client_name <- String.copy s),
         "<name>  Set executable for clients" ]

let parse_configuration ?(key_prefix="-join") ?(merge_args=false) ?(argv=Sys.argv) ?(start=0) speclist anon_fun usage_msg =
  try
    let res, args = make_configuration ~key_prefix:key_prefix () in
    let args' =
      if merge_args then
        List.merge (fun (x, _, _) (y, _, _) -> compare x y) speclist args
      else
        speclist @ args in
    Arg.parse_argv ~current:(ref start) argv args' anon_fun usage_msg;
    res
  with
  | Arg.Bad msg ->
      prerr_string msg;
      exit 2
  | Arg.Help msg ->
      print_string msg;
      exit 0


(* Client-related functions *)

type 'a lookup_function = Join.Ns.t -> string -> 'a

let lookup_once = Join.Ns.lookup

let rec lookup_times n w ns k =
  try
    lookup_once ns k
  with
  | Not_found ->
      if n = 1 then
        raise Not_found
      else begin
        Thread.delay w;
        let n' = if n = min_int then min_int else pred n in
        lookup_times n' w ns k
      end

type at_fail_chan = unit Join.chan

let do_at_fail f =
  def ch () = (try f () with _ -> ()); 0 in
  ch

def do_nothing_at_fail () = 0

let exit_at_fail_with_code c =
  def exit_at_fail () = exit c; 0 in
  exit_at_fail

let exit_at_fail = exit_at_fail_with_code 0

exception Invalid_magic of string * string

let init_client ?(at_fail=do_nothing_at_fail) ?(lookup=lookup_once) cfg =
  let inet_addr = (Unix.gethostbyname cfg.host).Unix.h_addr_list.(0) in
  let server_addr = Unix.ADDR_INET (inet_addr, cfg.port) in
  let server_site = Join.Site.there server_addr in
  let ns = Join.Ns.of_site server_site in
  Join.Site.at_fail server_site at_fail;
  let magic : string = Obj.magic (lookup ns cfg.magic_id) in
  if magic <> cfg.magic_value then
    raise (Invalid_magic (cfg.magic_value, magic));
  ignore (do_forks cfg.client_name cfg.fork_args (max 0 cfg.clients));
  ns

let init_client_with_lookup ?(at_fail=do_nothing_at_fail) ?(lookup=lookup_once) cfg id =
  let ns = init_client ~at_fail:at_fail cfg in
  let v = lookup ns id in
  ns, v


(* Server-related functions *)

let init_server cfg =
  let inet_addr = (Unix.gethostbyname cfg.host).Unix.h_addr_list.(0) in
  let server_addr = Unix.ADDR_INET (inet_addr, cfg.port) in
  Join.Ns.register Join.Ns.here cfg.magic_id cfg.magic_value;
  Join.Site.listen server_addr;
  let pids = do_forks cfg.client_name cfg.fork_args (max 0 cfg.clients) in
  Join.Ns.here, pids

let init_server_with_register cfg id v =
  let ns, pids = init_server cfg in
  Join.Ns.register ns id v;
  ns, pids


(* Miscellaneous functions *)

let wait_forever () =
  def wait() & never_sent() = reply to wait in
  wait ()
