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

(* Module [Arg]: parsing of command line arguments *)

(* This module provides a general mechanism for extracting options and
   arguments from the command line to the program.
*)

(* Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    There are six types of keywords: [Unit], [Set], [Clear], [String],
    [Int], and [Float].  [Unit], [Set] and [Clear] keywords take no
    argument.  [String], [Int], and [Float] keywords take the following
    word on the command line as an argument.
    Arguments not preceded by a keyword are called anonymous arguments.
*)

(*  Examples ([cmd] is assumed to be the command name):
-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd a b c           ](three anonymous arguments: ["a"], ["b"], and ["c"])
*)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
        (* The concrete type describing the behavior associated
           with a keyword. *)

val parse : (string * spec * string) list -> (string -> unit) -> string -> unit
(*
    [parse speclist anonfun usage_msg] parses the command line.
    [speclist] is a list of triples [(key, spec, doc)].
    [key] is the option keyword, it must start with a ['-'] character.
    [spec] gives the option type and the function to call when this option
    is found on the command line.
    [doc] is a one-line description of this option.
    [anonfun] is called on anonymous arguments.
    The functions in [spec] and [anonfun] are called in the same order
    as their arguments appear on the command line.

    If an error occurs, [parse] exits the program, after printing an error
    message as follows:
-   The reason for the error: unknown option, invalid or missing argument, etc.
-   [usage_msg]
-   The list of options, each followed by the corresponding [doc] string.

    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("--", String anonfun, doc)] in [speclist].

    By default, [parse] recognizes a unit option [-help], which will
    display [usage_msg] and the list of options, and exit the program.
    You can override this behaviour by specifying your own [-help]
    option in [speclist].
*)

exception Bad of string
(*
     Functions in [spec] or [anonfun] can raise [Bad] with an error
     message to reject invalid arguments.
*)

val usage: (string * spec * string) list -> string -> unit
(*
    [usage speclist usage_msg]
    [speclist] and [usage_msg] are the same as for [parse].  [usage]
    prints the same error message that [parse] prints in case of error.
*)

val current: int ref;;
(*
    Position (in [Sys.argv]) of the argument being processed.  You can
    change this value, e.g. to force [parse] to skip some arguments.
*)
