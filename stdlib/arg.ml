(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type spec =
    Unit of (unit -> unit)     (* Call the function with no argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)

exception Bad of string

type error =
    Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

open Printf

let stop error =
  let progname =
    if Array.length Sys.argv > 0 then Sys.argv.(0) else "(?)" in
  begin match error with
      Unknown s ->
        eprintf "%s: unknown option `%s'.\n" progname s
    | Missing s ->
        eprintf "%s: option `%s' needs an argument.\n" progname s
    | Wrong (opt, arg, expected) ->
        eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
                progname arg opt expected
    | Message s ->
        eprintf "%s: %s.\n" progname s
  end;
  exit 2

let parse speclist anonfun =
  let rec p = function
      [] -> ()
    | s :: t ->
        if String.length s >= 1 & String.get s 0 = '-'
        then do_key s t
        else begin try (anonfun s); p t with Bad m -> stop (Message m) end
  and do_key s l =    
    let action =
      try
        List.assoc s speclist
      with Not_found ->
        stop (Unknown s) in
    try
      match (action, l) with
        (Unit f, l) -> f (); p l
      | (Set r, l) -> r := true; p l
      | (Clear r, l) -> r := false; p l
      | (String f, arg::t) -> f arg; p t
      | (Int f, arg::t) ->
          begin try f (int_of_string arg)
          with Failure "int_of_string" -> stop (Wrong (s, arg, "an integer"))
          end;
          p t
      | (Float f, arg::t) -> f (float_of_string arg); p t
      | (_, []) -> stop (Missing s)
    with Bad m -> stop (Message m)
  in
    match Array.to_list Sys.argv with
      [] -> ()
    | a::l -> p l
