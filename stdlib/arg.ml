(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Set_string of string ref   (** Set the reference to the string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Set_int of int ref         (** Set the reference to the int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Set_float of float ref     (** Set the reference to the float argument *)
  | Symbol of string list * (string -> unit)
                               (** Take one of the symbols as argument and
                                   call the function with the symbol. *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

exception Bad of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3) :: t when y1 = x -> y2
  | _ :: t -> assoc3 x t
;;

let make_symlist prefix sep suffix l =
  match l with
  | [] -> "<none>"
  | h::t -> (List.fold_left (fun x y -> x ^ sep ^ y) (prefix ^ h) t) ^ suffix
;;

let print_spec (key, spec, doc) =
  match spec with
  | Symbol (l, _) -> eprintf "  %s %s %s\n" key (make_symlist "{" "|" "}" l) doc
  | _ -> eprintf "  %s %s\n" key doc
;;

let usage speclist errmsg =
  eprintf "%s\n" errmsg;
  List.iter print_spec speclist;
  try ignore (assoc3 "-help" speclist)
  with Not_found -> eprintf "  -help   Display this list of options\n";
  try ignore (assoc3 "--help" speclist)
  with Not_found -> eprintf "  --help  Display this list of options\n";
;;

let current = ref 0;;

let parse_argv argv speclist anonfun errmsg =
  let initpos = !current in
  let stop error =
    let progname =
      if initpos < Array.length argv then argv.(initpos) else "(?)" in
    begin match error with
      | Unknown "-help" -> ()
      | Unknown "--help" -> ()
      | Unknown s ->
          eprintf "%s: unknown option `%s'.\n" progname s
      | Missing s ->
          eprintf "%s: option `%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
                  progname arg opt expected
      | Message s ->
          eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    if error = Unknown "-help" || error = Unknown "--help"
    then exit 0
    else exit 2
  in
  let l = Array.length argv in
  incr current;
  while !current < l do
    let s = argv.(!current) in
    if String.length s >= 1 && String.get s 0 = '-' then begin
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        match action with
        | Unit f -> f ();
        | Set r -> r := true;
        | Clear r -> r := false;
        | String f when !current + 1 < l ->
            f argv.(!current + 1);
            incr current;
        | Symbol (symb, f) when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            if List.mem arg symb then begin
              f argv.(!current + 1);
              incr current;
            end else begin
              stop (Wrong (s, arg, "one of: " ^ (make_symlist "" " " "" symb)))
            end
        | Set_string r when !current + 1 < l ->
            r := argv.(!current + 1);
            incr current;
        | Int f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (int_of_string arg)
            with Failure "int_of_string" -> stop (Wrong (s, arg, "an integer"))
            end;
            incr current;
        | Set_int r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (int_of_string arg)
            with Failure "int_of_string" -> stop (Wrong (s, arg, "an integer"))
            end;
            incr current;
        | Float f when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try f (float_of_string arg);
            with Failure "float_of_string" -> stop (Wrong (s, arg, "a float"))
            end;
            incr current;
        | Set_float r when !current + 1 < l ->
            let arg = argv.(!current + 1) in
            begin try r := (float_of_string arg);
            with Failure "float_of_string" -> stop (Wrong (s, arg, "a float"))
            end;
            incr current;
        | Rest f ->
            while !current < l - 1 do
              f argv.(!current + 1);
              incr current;
            done;
        | _ -> stop (Missing s)
      with Bad m -> stop (Message m);
      end;
      incr current;
    end else begin
      (try anonfun s with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;

let parse = parse_argv Sys.argv;;
