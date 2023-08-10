(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                                                                        *)
(*   Copyright 2021 Indian Institute of Technology, Madras                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@alert unstable
    "The Effect interface may change in incompatible ways in the future."
]

(** Effects.

    See 'Language extensions/Effect handlers' section in the manual.

    @since 5.0 *)

type _ t
(** The type of effects. *)

exception Unhandled : 'a t * ('k, 'a) operation -> exn
(** [Unhandled(e, op)] is raised when operation [op] of effect [e] is
    performed and there is no handler for it. *)

exception Continuation_already_resumed
(** Exception raised when a continuation is continued or discontinued more
    than once. *)

val create : ?name:string -> unit -> 'e t
(** [create ?name ()] creates a fresh effect. [name] is used for
    printing error messages. *)

val equal : 'a t -> 'b t -> ('a, 'b) Type.eq option

val perform : 'e t -> ('o, 'e) operation -> 'o
(** [perform e op] performs an operation [op] of effect [e].

    @raise Unhandled if there is no handler for [e]. *)

type ('a,'b) continuation
(** [('a,'b) continuation] is a delimited continuation that expects a ['a]
    value and returns a ['b] value. *)

val continue : ('a, 'b) continuation -> 'a -> 'b
(** [continue k v] resumes the continuation [k] with value [v].

    @raise Continuation_already_resumed if [k] has already been resumed. *)

val discontinue : ('a, 'b) continuation -> exn -> 'b
(** [discontinue k e] resumes the continuation [k] by raising the
    exception [e].

    @raise Continuation_already_resumed if [k] has already been resumed.
 *)

val discontinue_with_backtrace :
  ('a, 'b) continuation -> exn -> Printexc.raw_backtrace -> 'b
(** [discontinue_with k e bt] resumes the continuation [k] by raising the
    exception [e] using the raw backtrace [bt] as the origin of the
    exception.

    @raise Continuation_already_resumed if [k] has already been resumed.
 *)

val reperform : 'e t -> ('o, 'e) operation -> ('o, 'a) continuation -> 'a
(** [reperform e op k] performs an operation [op] of effect [e] and
    resumes the continuation [k] with result.

    @raise Unhandled if there is no handler for [e].
    @raise Continuation_already_resumed if [k] has already been resumed. *)

val get_callstack : ('a,'b) continuation -> int -> Printexc.raw_backtrace
(** [get_callstack c n] returns a description of the top of the call stack on
    the continuation [c], with at most [n] entries. *)

type ('a, 'e) step =
  | Result of 'a
  | Exn of exn
  | Operation :
      ('o, 'e) operation * ('o, ('a, 'e) step) continuation -> ('a, 'e) step
(** [('a, 'e) step] is the result of running an effect handler until it
    returns a result, raises an exception or performs an operation. *)

val run : 'e t -> ('a -> 'b) -> 'a -> ('b, 'e) step
(** [run e f v] runs the computation [f v] handling effect [e]. *)

val fiber : 'e t -> ('a -> 'b) -> ('a, ('b, 'e) step) continuation
(** [fiber e f] creates a continuation that will run [f] handling
    effect [e]. *)

type ('a, 'e, 'b) handler =
  { result: 'a -> 'b;
    exn: exn -> 'b;
    operation: 'k. ('k, 'e) operation -> ('k, 'b) continuation -> 'b }
(** [('a, 'b) handler] is a handler record with three fields -- [result]
    is the value handler, [exn] handles exceptions, and [operation] handles the
    operations. *)  

val run_with : 'e t -> ('a -> 'b) -> 'a -> ('b, 'e, 'c) handler -> 'c
(** [run_with e f v h] runs the computation [f v] handling the effect
    [e] with handler [h]. *)

(* This should be [:=], but ocamldoc doesn't understand that. *)
type 'a eff = 'a t

module Legacy : sig

  type _ t = ..
  (** The type of effects. *)

  type legacy = effect
    | Legacy : 'a t -> 'a

  val legacy : legacy eff

  exception Unhandled : 'a t -> exn
  (** [Unhandled e] is raised when effect [e] is performed and there is no
      handler for it. *)

  exception Continuation_already_resumed
  (** Exception raised when a continuation is continued or discontinued more
      than once. *)

  val perform : 'a t -> 'a
  (** [perform e] performs an effect [e].

      @raise Unhandled if there is no handler for [e]. *)

  module Deep : sig
    (** Deep handlers *)

    type ('a,'b) continuation
    (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
        value and returns a ['b] value. *)

    val continue: ('a, 'b) continuation -> 'a -> 'b
    (** [continue k x] resumes the continuation [k] by passing [x] to [k].

        @raise Continuation_already_resumed if the continuation has already been
        resumed. *)

    val discontinue: ('a, 'b) continuation -> exn -> 'b
    (** [discontinue k e] resumes the continuation [k] by raising the
        exception [e] in [k].

        @raise Continuation_already_resumed if the continuation has already been
        resumed. *)

    val discontinue_with_backtrace:
      ('a, 'b) continuation -> exn -> Printexc.raw_backtrace -> 'b
    (** [discontinue_with_backtrace k e bt] resumes the continuation [k] by
        raising the exception [e] in [k] using [bt] as the origin for the
        exception.

        @raise Continuation_already_resumed if the continuation has already been
        resumed. *)

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c t -> (('c,'b) continuation -> 'b) option }
    (** [('a,'b) handler] is a handler record with three fields -- [retc]
        is the value handler, [exnc] handles exceptions, and [effc] handles the
        effects performed by the computation enclosed by the handler. *)

    val match_with: ('c -> 'a) -> 'c -> ('a,'b) handler -> 'b
    (** [match_with f v h] runs the computation [f v] in the handler [h]. *)

    type 'a effect_handler =
      { effc: 'b. 'b t -> (('b, 'a) continuation -> 'a) option }
    (** ['a effect_handler] is a deep handler with an identity value handler
        [fun x -> x] and an exception handler that raises any exception
        [fun e -> raise e]. *)

    val try_with: ('b -> 'a) -> 'b -> 'a effect_handler -> 'a
    (** [try_with f v h] runs the computation [f v] under the handler [h]. *)

    val get_callstack : ('a,'b) continuation -> int -> Printexc.raw_backtrace
    (** [get_callstack c n] returns a description of the top of the call stack on
        the continuation [c], with at most [n] entries. *)
  end

  module Shallow : sig
    (* Shallow handlers *)

    type ('a,'b) continuation
    (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
        value and returns a ['b] value. *)

    val fiber : ('a -> 'b) -> ('a, 'b) continuation
    (** [fiber f] constructs a continuation that runs the computation [f]. *)

    type ('a,'b) handler =
      { retc: 'a -> 'b;
        exnc: exn -> 'b;
        effc: 'c.'c t -> (('c,'a) continuation -> 'b) option }
    (** [('a,'b) handler] is a handler record with three fields -- [retc]
        is the value handler, [exnc] handles exceptions, and [effc] handles the
        effects performed by the computation enclosed by the handler. *)

    val continue_with : ('c,'a) continuation -> 'c -> ('a,'b) handler -> 'b
    (** [continue_with k v h] resumes the continuation [k] with value [v] with
        the handler [h].

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
     *)

    val discontinue_with : ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b
    (** [discontinue_with k e h] resumes the continuation [k] by raising the
        exception [e] with the handler [h].

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
     *)

    val discontinue_with_backtrace :
      ('a,'b) continuation -> exn -> Printexc.raw_backtrace ->
      ('b,'c) handler -> 'c
    (** [discontinue_with k e bt h] resumes the continuation [k] by raising the
        exception [e] with the handler [h] using the raw backtrace [bt] as the
        origin of the exception.

        @raise Continuation_already_resumed if the continuation has already been
        resumed.
     *)

    val get_callstack : ('a,'b) continuation -> int -> Printexc.raw_backtrace
    (** [get_callstack c n] returns a description of the top of the call stack on
        the continuation [c], with at most [n] entries. *)
  end

end
