(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t =
  | Partial_match                    (* P *)
  | Unused_match                     (* U *)
  | Method_override of string list   (* M *)
  | Hide_instance_variable of string (* V *)
  | Partial_application              (* F *)
  | Statement_type                   (* S *)
  | Other of string                  (* X *)
;;

let pflag = ref true;;
let uflag = ref true;;
let mflag = ref true;;
let vflag = ref true;;
let fflag = ref true;;
let sflag = ref true;;
let xflag = ref true;;

let rec parse_options s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | 'P' -> pflag := true
    | 'p' -> pflag := false
    | 'U' -> uflag := true
    | 'u' -> uflag := false
    | 'M' -> mflag := true
    | 'm' -> mflag := false
    | 'V' -> vflag := true
    | 'v' -> vflag := false
    | 'F' -> fflag := true
    | 'f' -> fflag := false
    | 'S' -> sflag := true
    | 's' -> sflag := false
    | 'X' -> xflag := true
    | 'x' -> xflag := false
    | 'A' -> parse_options "PUMVFSX"
    | 'a' -> parse_options "pumvfsx"
    | c -> raise (Arg.Bad (Printf.sprintf "unknown warning option %c" c))
  done
;;

let is_active = function
  | Partial_match -> !pflag
  | Unused_match -> !uflag
  | Method_override slist -> !mflag
  | Hide_instance_variable string -> !vflag
  | Partial_application -> !fflag
  | Statement_type -> !sflag
  | Other _ -> !xflag
;;

let message = function
  | Partial_match -> "this pattern-matching is not exhaustive."
  | Unused_match -> "this match case is unused."
  | Method_override slist ->
      String.concat " "
        ("the following methods are overriden \
          by the inherited class:\n " :: slist)
  | Hide_instance_variable lab ->
      "this definition of an instance variable " ^ lab ^
      " hides a previously\ndefined instance variable of the same name."
  | Partial_application ->
      "this function application is partial,\n\
       maybe some arguments are missing."
  | Statement_type ->
      "this expression should have type unit."
  | Other s -> s
;;
