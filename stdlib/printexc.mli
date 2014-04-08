(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Facilities for printing exceptions and inspecting current call stack. *)

val to_string: exn -> string
(** [Printexc.to_string e] returns a string representation of
   the exception [e]. *)

val print: ('a -> 'b) -> 'a -> 'b
(** [Printexc.print fn x] applies [fn] to [x] and returns the result.
   If the evaluation of [fn x] raises any exception, the
   name of the exception is printed on standard error output,
   and the exception is raised again.
   The typical use is to catch and report exceptions that
   escape a function application. *)

val catch: ('a -> 'b) -> 'a -> 'b
(** [Printexc.catch fn x] is similar to {!Printexc.print}, but
   aborts the program with exit code 2 after printing the
   uncaught exception.  This function is deprecated: the runtime
   system is now able to print uncaught exceptions as precisely
   as [Printexc.catch] does.  Moreover, calling [Printexc.catch]
   makes it harder to track the location of the exception
   using the debugger or the stack backtrace facility.
   So, do not use [Printexc.catch] in new code.  *)

val print_backtrace: out_channel -> unit
(** [Printexc.print_backtrace oc] prints an exception backtrace
    on the output channel [oc].  The backtrace lists the program
    locations where the most-recently raised exception was raised
    and where it was propagated through function calls.
    @since 3.11.0
*)

val get_backtrace: unit -> string
(** [Printexc.get_backtrace ()] returns a string containing the
    same exception backtrace that [Printexc.print_backtrace] would
    print.
    @since 3.11.0
*)

val record_backtrace: bool -> unit
(** [Printexc.record_backtrace b] turns recording of exception backtraces
    on (if [b = true]) or off (if [b = false]).  Initially, backtraces
    are not recorded, unless the [b] flag is given to the program
    through the [OCAMLRUNPARAM] variable.
    @since 3.11.0
*)

val backtrace_status: unit -> bool
(** [Printexc.backtrace_status()] returns [true] if exception
    backtraces are currently recorded, [false] if not.
    @since 3.11.0
*)

val register_printer: (exn -> string option) -> unit
(** [Printexc.register_printer fn] registers [fn] as an exception
    printer.  The printer should return [None] or raise an exception
    if it does not know how to convert the passed exception, and [Some
    s] with [s] the resulting string if it can convert the passed
    exception. Exceptions raised by the printer are ignored.

    When converting an exception into a string, the printers will be invoked
    in the reverse order of their registrations, until a printer returns
    a [Some s] value (if no such printer exists, the runtime will use a
    generic printer).

    When using this mechanism, one should be aware that an exception backtrace
    is attached to the thread that saw it raised, rather than to the exception
    itself. Practically, it means that the code related to [fn] should not use
    the backtrace if it has itself raised an exception before.
    @since 3.11.2
*)

(** {6 Raw backtraces} *)

type raw_backtrace_slot
type raw_backtrace = raw_backtrace_slot array

(** The abstract type [raw_backtrace_slot] stores a slot of a backtrace in
    a low-level format, instead of directly exposing them as string as
    the [get_backtrace()] function does.

    This allows delaying the formatting of backtraces to when they are
    actually printed, which might be useful if you record more
    backtraces than you print.

    Elements of type raw_backtrace_slot can be compared and hashed: when two
    elements are equal, then they represent the same source location (the
    converse is not necessarily true in presence of inlining, for example).
*)

val get_raw_backtrace: unit -> raw_backtrace
val print_raw_backtrace: out_channel -> raw_backtrace -> unit
val raw_backtrace_to_string: raw_backtrace -> string

(** {6 Backtrace slots processing} *)

type backtrace_slot =
  | Known_location of bool   (* is_raise *)
                    * string (* filename *)
                    * int    (* line number *)
                    * int    (* start char *)
                    * int    (* end char *)
  | Unknown_location of bool (*is_raise*)

(** [convert_raw_backtrace_slot] converts one slot of a raw backtrace
    to an Ocaml algebraic datatype representing to location
    information in the source file.

    Raises [Failure] if not able to load debug information.
*)
val convert_raw_backtrace_slot: raw_backtrace_slot -> backtrace_slot

(** [format_backtrace_slot pos slot] returns the string
    representation of the backtrace slot [slot] as
    [raw_backtrace_to_string] would format it, assuming it is the
    [pos]-th element of the backtrace: the 0-th element is
    pretty-printed differently than the other.

    Note that Printexc's printing function will skip any slot equal to
    [Unknown_location true]; you should as well if you wish to
    reproduce its behavior.
*)
val format_backtrace_slot : int -> backtrace_slot -> string

(** {6 Current call stack} *)

val get_callstack: int -> raw_backtrace

(** [Printexc.get_callstack n] returns a description of the top of the
    call stack on the current program point (for the current thread),
    with at most [n] entries.  (Note: this function is not related to
    exceptions at all, despite being part of the [Printexc] module.)

    @since 4.01.0
*)


(** {6 Exception slots} *)

val exn_slot_id: exn -> int
(** [Printexc.exn_slot_id] returns an integer which uniquely identifies
    the constructor used to create the exception value [exn]
    (in the current runtime).

    @since 4.02.0
*)

val exn_slot_name: exn -> string
(** [Printexc.exn_slot_id exn] returns the internal name of the constructor
    used to create the exception value [exn].

    @since 4.02.0
*)


