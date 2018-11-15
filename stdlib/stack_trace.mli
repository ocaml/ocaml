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

(** Stack traces.

    Facilities for getting the current stack trace, printing and inspecting
    them.

    @since 4.08 *)

type t
(** The type of stack traces.

    Stack traces cannot be marshalled. If you need marshalling, you should use
    the array returned by the {!frames} function. *)

val current: int -> t
(** [current n] returns a description of the stack trace on the current program
    point (for the current thread), with at most [n] entries. *)

val to_string: t -> string
(** Return a string listing the program locations contained in the stack
    trace. *)

val length: t -> int
(** [length st] returns the number of (non-inlined) frames in the stack trace
    [st]. *)

module Frame : sig
  (** {1 Manipulation of stack trace information}

      These functions are used to traverse the frames of a stack trace and
      extract information from them in a programmer-friendly format. *)

  type stack_trace

  type t
  (** The abstract type [t] represents a single frame of a stack trace. *)

  val is_raise: t -> bool
  (** [is_raise frame] is [true] when [frame] refers to a raising point in the
      code, and [false] when it comes from a simple function call. *)

  val is_inline: t -> bool
  (** [is_inline frame] is [true] when [frame] refers to a call that got inlined
      by the compiler, and [false] when it comes from any other context. *)

  type location =
    { filename: string;
      line_number: int;
      start_char: int;
      end_char: int }
  (** The type of location information found in stack traces. [start_char] and
      [end_char] are positions relative to the beginning of the line. *)

  val location: t -> location option
  (** [location frame] returns the location information of the frame,
      if available, and [None] otherwise.

      Some possible reasons for failing to return a location are as follow:
      - the frame corresponds to a compiler-inserted raise
      - the frame corresponds to a part of the program that has not been
      compiled with debug information ([-g]) *)

  val format: int -> t -> string option
  (** [format pos frame] returns the string representation of [frame] as
      {!to_string} would format it, assuming it is the [pos]-th element of the
      callstack: the [0]-th element is pretty-printed differently than the
      others.

      Whole-stack trace printing functions also skip some uninformative frames;
      in that case, [format pos frame] returns [None]. *)

  module Raw : sig
    (** {1 Raw stack trace frames} *)

    type t
    (** This type allows direct access to raw stack trace frames, without any
        conversion in an OCaml-usable data-structure. Being process-specific,
        they must absolutely not be marshalled, and are unsafe to use for this
        reason (marshalling them may not fail, but un-marshalling and using the
        result will result in undefined behavior).

        Elements of this type can still be compared and hashed: when two
        elements are equal, then they represent the same source location (the
        converse is not necessarily true in presence of inlining, for
        example). *)

    val get: stack_trace -> int -> t
    (** [get st pos] returns the frame in position [pos] in the stack trace
        [st]. *)

    val next: t -> t option
    (** [next frame] returns the next inlined frame, if any.

        Sample code to iterate over all frames (inlined and non-inlined):
        {[
          (* Iterate over inlined frames *)
          let rec iter_callstack_frame f frame =
            f frame;
            match Frame.Raw.next frame with
            | None -> ()
            | Some frame' -> iter_callstack_frame f frame'

          (* Iterate over stack frames *)
          let iter_callstack f bt =
            for i = 0 to length bt - 1 do
              iter_callstack_frame f (Frame.get bt i)
            done
        ]}
    *)
  end

  val of_raw: Raw.t -> t
  (** Extracts the user-friendly strack trace frame from a low-level raw
      frame. *)
end with type stack_trace := t

val frames: t -> Frame.t array option
(** Returns the frames of a stack trace, or [None] if none of them contain
    useful information.

    In the return array, the frame at index [0] corresponds to the most recent
    function call or raise.

    Some possible reasons for returning [None] are as follow:
    - none of the frames in the trace come from modules compiled with
      debug information ([-g])
    - the program is a bytecode program that has not been linked with
      debug information enabled ([ocamlc -g]) *)
