(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Exception values.

    Facilities for printing exceptions and obtaining traces.

    @since 4.08 *)

type t = exn = ..
(** The type for exceptions. *)

val id: t -> int
(** [id exn] returns an integer which uniquely identifies the constructor used
    to create the exception value [exn] (in the current runtime).  *)

val name: t -> string
(** [name exn] returns the internal name of the constructor used to create the
    exception value [exn].  *)

val to_string: t -> string
(** [to_string e] returns a string representation of the exception [e]. *)

(** {1:tracing Tracing} *)

val set_tracing: bool -> unit
(** [set_recording_status b] turns recording of exception traces on (if [b
    = true]) or off (if [b = false]).  Initially, traces are not recorded,
    unless the [b] flag is given to the program through the [OCAMLRUNPARAM]
    variable. *)

val tracing: unit -> bool
(** Returns [true] if exception traces are currently recorded, [false] if
    not. *)

val last_trace: unit -> Stack_trace.t
(** Returns a stack trace containing the program locations where the
    most-recently raised exception was raised and where it was propagated
    through function calls.

    If the call is not inside an exception handler, the returned trace is
    unspecified. If the call is after some exception-catching code (before in
    the handler, or in a when-guard during the matching of the exception
    handler), the stack trace may correspond to a later exception than the
    handled one. *)

(** {1:raising Raising} *)

external raise: t -> 'a = "%raise"

external reraise: t -> 'a = "%reraise"

external raise_notrace: t -> 'a = "%raise_notrace"

external raise_with_trace: t -> Stack_trace.t -> 'a = "%raise_with_backtrace"
(** Reraise the exception using the given trace for the origin of the
    exception. *)

(** {1 Custom printers} *)

val register_printer: (t -> string option) -> unit
(** [register_printer fn] registers [fn] as an exception printer.  The printer
    should return [None] or raise an exception if it does not know how to
    convert the passed exception, and [Some s] with [s] the resulting string if
    it can convert the passed exception. Exceptions raised by the printer are
    ignored.

    When converting an exception into a string, the printers will be invoked in
    the reverse order of their registrations, until a printer returns a [Some s]
    value (if no such printer exists, the runtime will use a generic printer).

    When using this mechanism, one should be aware that an exception stack trace
    is attached to the thread that saw it raised, rather than to the exception
    itself. Practically, it means that the code related to [fn] should not use
    the stack trace if it has itself raised an exception before. *)

(** {1 Uncaught exceptions} *)

val set_uncaught_exception_handler: (t -> Stack_trace.t -> unit) -> unit
(** [Printexc.set_uncaught_exception_handler fn] registers [fn] as the handler
    for uncaught exceptions. The default handler prints the exception and the
    stack trace on standard error output.

    Note that when [fn] is called all the functions registered with
    {!Stdlib.at_exit} have already been called. Because of this you must
    make sure any output channel [fn] writes on is flushed.

    Also note that exceptions raised by user code in the interactive toplevel
    are not passed to this function as they are caught by the toplevel itself.

    If [fn] raises an exception, both the exceptions passed to [fn] and raised
    by [fn] will be printed with their respective stack traces. *)
