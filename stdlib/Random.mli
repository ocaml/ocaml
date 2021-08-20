(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Pseudo-random number generators (PRNG). *)

(** {1 Basic functions} *)

val init : int -> unit
(** Initialize the generator, using the argument as a seed.
     The same seed will always yield the same sequence of numbers. *)

val full_init : int array -> unit
(** Same as {!Random.init} but takes more data as seed. *)

val self_init : unit -> unit
(** Initialize the generator with a random seed chosen
   in a system-dependent way.  If [/dev/urandom] is available on
   the host machine, it is used to provide a highly random initial
   seed.  Otherwise, a less random seed is computed from system
   parameters (current time, process IDs). *)

val bits : unit -> int
(** Return 30 random bits in a nonnegative integer.
    @before 3.12.0 used a different algorithm (affects all the following
                   functions)
*)

val int : int -> int
(** [Random.int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0 and less
     than 2{^30}. *)

val full_int : int -> int
(** [Random.full_int bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive). [bound] may be any positive integer.

     If [bound] is less than 2{^30}, [Random.full_int bound] is equal to
     {!Random.int}[ bound]. If [bound] is greater than 2{^30} (on 64-bit systems
     or non-standard environments, such as JavaScript), [Random.full_int]
     returns a value, where {!Random.int} raises {!Invalid_argument}.

    @since 4.13.0 *)

val int32 : Int32.t -> Int32.t
(** [Random.int32 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val nativeint : Nativeint.t -> Nativeint.t
(** [Random.nativeint bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val int64 : Int64.t -> Int64.t
(** [Random.int64 bound] returns a random integer between 0 (inclusive)
     and [bound] (exclusive).  [bound] must be greater than 0. *)

val float : float -> float
(** [Random.float bound] returns a random floating-point number
   between 0 and [bound] (inclusive).  If [bound] is
   negative, the result is negative or zero.  If [bound] is 0,
   the result is 0. *)

val bool : unit -> bool
(** [Random.bool ()] returns [true] or [false] with probability 0.5 each. *)

val bits32 : unit -> Int32.t
(** [Random.bits32 ()] returns 32 random bits as an integer between
    {!Int32.min_int} and {!Int32.max_int}.
    @since 4.14.0 *)

val bits64 : unit -> Int64.t
(** [Random.bits64 ()] returns 64 random bits as an integer between
    {!Int64.min_int} and {!Int64.max_int}.
    @since 4.14.0 *)

val nativebits : unit -> Nativeint.t
(** [Random.nativebits ()] returns 32 or 64 random bits (depending on
    the bit width of the platform) as an integer between
    {!Nativeint.min_int} and {!Nativeint.max_int}.
    @since 4.14.0 *)

(** {1 Advanced functions} *)

(** The functions from module {!State} manipulate the current state
    of the random generator explicitly.
    This allows using one or several deterministic PRNGs,
    even in a multi-threaded program, without interference from
    other parts of the program.
*)

module State : sig
  type t
  (** The type of PRNG states. *)

  val make : int array -> t
  (** Create a new state and initialize it with the given seed. *)

  val make_self_init : unit -> t
  (** Create a new state and initialize it with a system-dependent
      low-entropy seed. *)

  val copy : t -> t
  (** Return a copy of the given state. *)

  val bits : t -> int
  val int : t -> int -> int
  val full_int : t -> int -> int
  val int32 : t -> Int32.t -> Int32.t
  val nativeint : t -> Nativeint.t -> Nativeint.t
  val int64 : t -> Int64.t -> Int64.t
  val float : t -> float -> float
  val bool : t -> bool
  val bits32 : t -> Int32.t
  val bits64 : t -> Int64.t
  val nativebits : t -> Nativeint.t
  (** These functions are the same as the basic functions, except that they
      use (and update) the given PRNG state instead of the default one.
  *)
end


val get_state : unit -> State.t
(** Return the current state of the generator used by the basic functions. *)

val set_state : State.t -> unit
(** Set the state of the generator used by the basic functions. *)
