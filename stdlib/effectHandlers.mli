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

type _ eff = ..
(* Type of effects *)

external perform : 'a eff -> 'a = "%perform"
(** [perform e] performs an effect [e].

    @raises Unhandled if there is no active handler. *)

module Deep : sig
  (** Deep handlers *)

  type ('a,'b) continuation
  (** [('a,'b) continuation] is a delimited continuation that expects a ['a]
      value and returns a ['b] value. *)

  val continue: ('a, 'b) continuation -> 'a -> 'b
  (** [continue k x] resumes the continuation [k] by passing [x] to [k].

      @raise Invalid_argument if the continuation has already been
      resumed. *)

  val discontinue: ('a, 'b) continuation -> exn -> 'b
  (** [discontinue k e] resumes the continuation [k] by raising the
      exception [e] in [k].

      @raise Invalid_argument if the continuation has already been
      resumed. *)

  type ('a,'b) handler =
    { retc: 'a -> 'b;
      exnc: exn -> 'b;
      effc: 'c.'c eff -> (('c,'b) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three field -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val match_with: ('a -> 'b) -> 'a -> ('b,'c) handler -> 'c
  (** [matchwith f v h] runs the computation [f v] in the handler [h]. *)

  type 'a effect_handler =
    { effc: 'b. 'b eff -> (('b, 'a) continuation -> 'a) option }
  (** ['a effect_handler] is a deep handler with an identity value handler
      [fun x -> x] and an exception handler that raises any exception
      [fun e -> raise e]. *)

  val try_with: ('a -> 'b) -> 'a -> 'b effect_handler -> 'b
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
      effc: 'c.'c eff -> (('c,'a) continuation -> 'b) option }
  (** [('a,'b) handler] is a handler record with three field -- [retc]
      is the value handler, [exnc] handles exceptions, and [effc] handles the
      effects performed by the computation enclosed by the handler. *)

  val continue_with : ('a,'b) continuation -> 'a -> ('b,'c) handler -> 'c
  (** [continue_with k v h] resumes the continuation [k] with value [v] with
      the handler [h].

      @raise Invalid_argument if the continuation has already been resumed.
   *)

  val discontinue_with : ('a,'b) continuation -> exn -> ('b,'c) handler -> 'c
  (** [discontinue_with k e h] resumes the continuation [k] by raising the
      exception [e] with the handler [h].

      @raise Invalid_argument if the continuation has already been resumed.
   *)

  external get_callstack :
    ('a,'b) continuation -> int -> Printexc.raw_backtrace =
    "caml_get_continuation_callstack"
  (** [get_callstack c n] returns a description of the top of the call stack on
      the continuation [c], with at most [n] entries. *)
end
