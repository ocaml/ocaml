(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                 Vesa Karvonen <vesa.a.j.k@gmail.com>                   *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This submodule is imported from the Backoff library:
     https://github.com/ocaml-multicore/backoff
   It is currently not exposed in the public interface, and reserved
   to the implementation of derived Atomic operations.

   To avoid dependency cycles within the runtime, we removed the use
   of Random.bits to introduce random jitter. Instead we now wait
   deterministically for 2^R loop iterations after R retries.
*)
module Backoff : sig
  (** Exponential backoff mechanism. *)

  type t [@@immediate]
  (** Type of backoff values. *)

  val max_wait_log : int
  (** Logarithm of the maximum allowed value for wait. *)

  val create : ?lower_wait_log:int -> ?upper_wait_log:int -> unit -> t
  (** [create] creates a backoff value. [upper_wait_log], [lower_wait_log]
      override the logarithmic upper and lower bound on the number of spins
      executed by {!once}. *)

  val default : t
  (** [default] is equivalent to [create ()]. *)

  val once : t -> t
  (** [once b] executes one wait and returns a new backoff with logarithm
      of the current maximum value incremented unless it is already at
      [upper_wait_log] of [b]. *)

  val reset : t -> t
  (** [reset b] returns a backoff equivalent to [b] except with
      current value set to the [lower_wait_log] of [b]. *)

  val get_wait_log : t -> int
  (** [get_wait_log b] returns logarithm of the maximum value of wait for next
      {!once}. *)
end = struct
  type t = int

  (* externals imported to avoid dependency cycles *)
  external bool_to_int : bool -> int = "%identity"
  external cpu_relax : unit -> unit
    = "caml_ml_domain_cpu_relax"
  external get_recommended_domain_count: unit -> int
    = "caml_recommended_domain_count" [@@noalloc]

  let single_mask = bool_to_int (get_recommended_domain_count () = 1) - 1
  let bits = 5
  let max_wait_log = 30 (* [Random.bits] returns 30 random bits. *)
  let mask = (1 lsl bits) - 1

  let create ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
    assert (
      0 <= lower_wait_log
      && lower_wait_log <= upper_wait_log
      && upper_wait_log <= max_wait_log);
    (upper_wait_log lsl (bits * 2))
    lor (lower_wait_log lsl bits) lor lower_wait_log

  let get_upper_wait_log backoff = backoff lsr (bits * 2)
  let get_lower_wait_log backoff = (backoff lsr bits) land mask
  let get_wait_log backoff = backoff land mask

  let reset backoff =
    let lower_wait_log = get_lower_wait_log backoff in
    backoff land lnot mask lor lower_wait_log

  (* We don't want [once] to be inlined.  This may avoid code bloat. *)
  let[@inline never] once backoff =
    let wait_log = get_wait_log backoff in
    let wait_mask = (1 lsl wait_log) - 1 in
    (* We use a ref and a countdown while-loop (uses one variable)
       instead of a for-loop (uses two variables) to reduce register
       pressure.  Local ref does not allocate with native compiler. *)
    let t = ref (wait_mask land single_mask) in
    while 0 <= !t do
      cpu_relax ();
      t := !t - 1
    done;
    let upper_wait_log = get_upper_wait_log backoff in
    (* We recompute [wait_log] to reduce register pressure. *)
    let wait_log = get_wait_log backoff in
    (* [bool_to_int] generates branchless code, this reduces branch predictor
       pressure and generates shorter code. *)
    let next_wait_log = wait_log + bool_to_int (wait_log < upper_wait_log) in
    backoff - wait_log + next_wait_log

  let default = create ()
end
[@@warning "-unused-value-declaration"]

module _ = Backoff

external ignore : 'a -> unit = "%ignore"

module Loc = struct
  type 'a t = 'a atomic_loc

  external get : 'a t -> 'a = "%atomic_load_loc"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange_loc"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"
  external fetch_and_add : int t -> int -> int = "%atomic_fetch_add_loc"

  let set t v =
    ignore (exchange t v)
  let incr t =
    ignore (fetch_and_add t 1)
  let decr t =
    ignore (fetch_and_add t (-1))

  let update f t =
    let rec loop ~backoff f t =
      let v_old = get t in
      let v_new = f v_old in
      if v_old == v_new || compare_and_set t v_old v_new
      then ()
      else loop ~backoff:(Backoff.once backoff) f t
    in loop ~backoff:Backoff.default f t
end

type !'a t =
  { mutable contents: 'a [@atomic];
  }

let make v =
  { contents = v }

external make_contended : 'a -> 'a t = "caml_atomic_make_contended"

let get t =
  t.contents
let set t v =
  t.contents <- v

let exchange t v =
  Loc.exchange [%atomic.loc t.contents] v
let compare_and_set t old new_ =
  Loc.compare_and_set [%atomic.loc t.contents] old new_
let fetch_and_add t incr =
  Loc.fetch_and_add [%atomic.loc t.contents] incr
let incr t =
  Loc.incr [%atomic.loc t.contents]
let decr t =
  Loc.decr [%atomic.loc t.contents]

let update f t =
  Loc.update f [%atomic.loc t.contents]

module Array = struct
  type !'a t =
    'a array

  external check_array_bound
    : 'a t -> int -> unit
    = "%check_array_bound"

  external unsafe_index
    : 'a t -> int -> 'a Loc.t
    = "%atomic_unsafe_index"

  external length
    : 'a array -> int
    = "%array_length"

  external uniform_array_make
    : int -> 'a -> 'a t
    = "caml_uniform_array_make"

  let[@inline] unsafe_get t i =
    Loc.get (unsafe_index t i)
  let[@inline] get t i =
    check_array_bound t i;
    unsafe_get t i

  let[@inline] unsafe_set t i v =
    (* using Loc.set works less well as Simplif misses the
       lambda-level elimination of the atomic-location pair. *)
    ignore (Loc.exchange (unsafe_index t i) v)
  let[@inline] set t i v =
    check_array_bound t i;
    unsafe_set t i v

  let[@inline] unsafe_exchange t i v =
    Loc.exchange (unsafe_index t i) v
  let[@inline] exchange t i v =
    check_array_bound t i;
    unsafe_exchange t i v

  let[@inline] unsafe_compare_and_set t i old new_ =
    Loc.compare_and_set (unsafe_index t i) old new_
  let[@inline] compare_and_set t i old new_ =
    check_array_bound t i;
    unsafe_compare_and_set t i old new_

  let[@inline] unsafe_fetch_and_add t i incr =
    Loc.fetch_and_add (unsafe_index t i) incr
  let[@inline] fetch_and_add t i incr =
    check_array_bound t i;
    unsafe_fetch_and_add t i incr

  let[@inline] unsafe_update f t i =
    Loc.update f (unsafe_index t i)
  let[@inline] update f t i =
    check_array_bound t i;
    unsafe_update f t i

  let make len v =
    if len < 0 then
      invalid_arg "Atomic.Array.make" ;
    uniform_array_make len v

  let init len fn =
    if len < 0 then
      invalid_arg "Atomic_array.init"
    else if len = 0 then
      [||]
    else begin
      let t = uniform_array_make len (fn 0) in
      for i = 1 to len - 1 do
        unsafe_set t i (fn i)
      done ;
      t
    end
end
