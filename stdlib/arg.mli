(* Module [Arg]: parsing of command line arguments *)

(* This module provides a general mechanism for extracting options and
   arguments from the command line to the program. *)

(* Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    There are four types of keywords: [Unit], [String], [Int], and [Float].
    [Unit] keywords do not take an argument.
    [String], [Int], and [Float] keywords take the following word on the
    command line as an argument.
    Arguments not preceded by a keyword are called anonymous arguments. *)

(*  Examples ([cmd] is assumed to be the command name):

-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd 1 2 3           ](three anonymous arguments: ["1"], ["2"], and ["3"])
-   [cmd 1 2 -flag 3 -string bar 4]
-   [                    ](four anonymous arguments, a unit option, and
-   [                    ] a string option with argument ["bar"])
*)

type spec =
    String of (string -> unit)
  | Int of (int -> unit)
  | Unit of (unit -> unit)
  | Float of (float -> unit)
        (* The concrete type describing the behavior associated
           with a keyword. *)

val parse : (string * spec) list -> (string -> unit) -> unit
        (* [parse speclist anonfun] parses the command line,
           calling the functions in [speclist] whenever appropriate,
           and [anonfun] on anonymous arguments.
           The functions are called in the same order as they appear
           on the command line.
           The strings in the [(string * spec) list] are keywords and must
           start with a [-], else they are ignored. *)

exception Bad of string
        (* Functions in [speclist] or [anonfun] can raise [Bad] with
           an error message to reject invalid arguments. *)
