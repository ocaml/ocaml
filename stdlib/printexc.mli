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

type raw_backtrace

(** The abstract type [backtrace] stores exception backtraces in
    a low-level format, instead of directly exposing them as string as
    the [get_backtrace()] function does.

    This allows to pay the performance overhead of representation
    conversion and formatting only at printing time, which is useful
    if you want to record more backtrace than you actually print.
*)

val get_raw_backtrace: unit -> raw_backtrace
val print_raw_backtrace: out_channel -> raw_backtrace -> unit
val raw_backtrace_to_string: raw_backtrace -> string


(** {6 Current call stack} *)

val get_callstack: int -> raw_backtrace

(** [Printexc.get_callstack n] returns a description of the top of the
    call stack on the current program point (for the current thread),
    with at most [n] entries.  (Note: this function is not related to
    exceptions at all, despite being part of the [Printexc] module.)

    @since 4.01.0
*)
