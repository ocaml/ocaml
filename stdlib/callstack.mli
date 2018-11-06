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

(** Call stacks.

    Facilities for getting the current call stack, printing and inspecting them.

    @since 4.08 *)

type t
(** The abstract type [t] stores a callstack.

    Callstacks cannot be marshalled. If you need marshalling, you should use the
    array returned by the {!slots} function. *)

val current: int -> t
(** [current n] returns a description of the top of the callstack on the current
    program point (for the current thread), with at most [n] entries. *)

val to_string: t -> string
(** Return a string from a callstack, listing the program locations contained in
    the callstack. *)

val length: t -> int
(** [length c] returns the number of slots in the callstack [c].  *)

module Slot : sig
  (** {1 Manipulation of callstack information}

      These functions are used to traverse the slots of a callstack and extract
      information from them in a programmer-friendly format. *)

  type callstack

  type t
  (** The abstract type [t] represents a single slot of a callstack. *)

  val is_raise: t -> bool
  (** [is_raise slot] is [true] when [slot] refers to a raising point in the
      code, and [false] when it comes from a simple function call. *)

  val is_inline: t -> bool
  (** [is_inline slot] is [true] when [slot] refers to a call that got inlined
      by the compiler, and [false] when it comes from any other context. *)

  type location =
    { filename: string;
      line_number: int;
      start_char: int;
      end_char: int }
  (** The type of location information found in callstacks. [start_char] and
      [end_char] are positions relative to the beginning of the line. *)

  val location: t -> location option
  (** [location slot] returns the location information of the slot,
      if available, and [None] otherwise.

      Some possible reasons for failing to return a location are as follow:
      - the slot corresponds to a compiler-inserted raise
      - the slot corresponds to a part of the program that has not been
      compiled with debug information ([-g]) *)

  val format: int -> t -> string option
  (** [format pos slot] returns the string representation of [slot] as
      {!to_string} would format it, assuming it is the [pos]-th element of the
      callstack: the [0]-th element is pretty-printed differently than the
      others.

      Whole-callstack printing functions also skip some uninformative slots; in
      that case, [format pos slot] returns [None]. *)

  module Raw : sig
    (** {1 Raw callstack slots} *)

    type t
    (** This type allows direct access to raw callstack slots, without any
        conversion in an OCaml-usable data-structure. Being process-specific,
        they must absolutely not be marshalled, and are unsafe to use for this
        reason (marshalling them may not fail, but un-marshalling and using the
        result will result in undefined behavior).

        Elements of this type can still be compared and hashed: when two
        elements are equal, then they represent the same source location (the
        converse is not necessarily true in presence of inlining, for
        example). *)

    val get: callstack -> int -> t
    (** [get c pos] returns the slot in position [pos] in the callstack [c]. *)

    val next: t -> t option
    (** [next slot] returns the next slot inlined, if any.

        Sample code to iterate over all frames (inlined and non-inlined):
        {[
          (* Iterate over inlined frames *)
          let rec iter_callstack_slot f slot =
            f slot;
            match Raw.next slot with
            | None -> ()
            | Some slot' -> iter_callstack_slot f slot'

          (* Iterate over stack frames *)
          let iter_callstack f bt =
            for i = 0 to length bt - 1 do
              iter_callstack_slot f (Slot.get bt i)
            done
        ]}
    *)
  end

  val of_raw: Raw.t -> t
  (** Extracts the user-friendly callstack slot from a low-level raw callstack
      slot. *)
end with type callstack := t

val slots: t -> Slot.t array option
(** Returns the slots of a callstack, or [None] if none of them contain useful
    information.

    In the return array, the slot at index [0] corresponds to the most recent
    function call or raise.

    Some possible reasons for returning [None] are as follow:
    - none of the slots in the trace come from modules compiled with
    debug information ([-g])
    - the program is a bytecode program that has not been linked with
    debug information enabled ([ocamlc -g]) *)
