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

(** Effects.

    @since 5.0 *)

type _ t = ..
(** The type of effects. *)

exception Unhandled : 'a t -> exn
(** Exception raised when an effect is performed and there is no matching
    handler for that effect. The argument is the unhandled effect. *)

exception Continuation_already_taken
(** Exception raised when a continuation is resumed with a [continue] or
    [discontinue] but the continuation has already been resumed. *)

external perform : 'a t -> 'a = "%perform"
(** [perform e] performs an effect [e].

    @raise Unhandled if there is no active handler. *)

module Deep : sig
  (** Deep handlers *)

  type ('a,'b) continuation
  (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
      value and returns a ['b] value. *)

  val continue: ('a, 'b) continuation -> 'a -> 'b
  (** [continue k x] resumes the continuation [k] by passing [x] to [k].

      @raise Continuation_already_taken if the continuation has already been
      resumed. *)

  val discontinue: ('a, 'b) continuation -> exn -> 'b
  (** [discontinue k e] resumes the continuation [k] by raising the
      exception [e] in [k].

      @raise Continuation_already_taken if the continuation has already been
      resumed. *)

  val discontinue_with_backtrace:
    ('a, 'b) continuation -> exn -> Printexc.raw_backtrace -> 'b
  (** [discontinue_with_backtrace k e bt] resumes the continuation [k] by
      raising the exception [e] in [k] using [bt] as the origin for the
      exception.

      @raise Continuation_already_taken if the continuation has already been
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

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
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

      @raise Continuation_already_taken if the continuation has already been
      resumed.
   *)

  val discontinue_with : ('c,'a) continuation -> exn -> ('a,'b) handler -> 'b
  (** [discontinue_with k e h] resumes the continuation [k] by raising the
      exception [e] with the handler [h].

      @raise Continuation_already_taken if the continuation has already been
      resumed.
   *)

  val discontinue_with_backtrace :
    ('a,'b) continuation -> exn -> Printexc.raw_backtrace ->
    ('b,'c) handler -> 'c
  (** [discontinue_with k e bt h] resumes the continuation [k] by raising the
      exception [e] with the handler [h] using the raw backtrace [bt] as the
      origin of the exception.

      @raise Continuation_already_taken if the continuation has already been
      resumed.
   *)

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)
end
