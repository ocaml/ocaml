(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type spec =
  | Unit of (unit -> unit)     (* Call the function with no argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)

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
  | (y1, y2, y3)::t when y1 = x -> y2
  | _::t -> assoc3 x t
;;

let print_doc =
  function (key, _, doc) -> eprintf "  %s %s\n" key doc
;;

let parse speclist anonfun errmsg =
  let stop error =
    (* Print the reason for the error *)
    let progname =
      if Array.length Sys.argv > 0 then Sys.argv.(0) else "(?)" in
    begin match error with
      | Unknown s when s = "-help" -> ()
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
    (* Print the usage message *)
    eprintf "%s\n" errmsg;
    List.iter print_doc speclist;
    exit 2
  in
  let rec p = function
    | [] -> ()
    | s :: t ->
        if String.length s >= 1 & String.get s 0 = '-'
        then do_key s t
        else begin try (anonfun s); p t with Bad m -> stop (Message m) end
  and do_key s l =
    let action =
      try assoc3 s speclist
      with Not_found -> stop (Unknown s) in
    try
      match (action, l) with
      | (Unit f, l) -> f (); p l
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
    | [] -> ()
    | a::l -> p l
