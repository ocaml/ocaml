(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Thread-local Storage.

    @since 5.2 *)

module Key : sig
  type 'a t
  (** Type of a TLS key *)

  val create : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a t
  (** [create f] returns a new key bound to initialiser [f] for accessing
      thread-local variables.

      If [split_from_parent] is not provided, the value for a new
      thread will be computed on-demand by the new thread: the first
      [get] call will call the initializer [f] and store that value.

      If [split_from_parent] is provided, spawning a domain or thread will
      derive the child value (for this key) from the parent
      value. This computation happens in the parent thread and it
      always happens, regardless of whether the child thread or
      domain will use it.
      If the splitting function is expensive or requires
      child-side computation, consider using ['a Lazy.t key]:

      {[
      let init () = ...

      let split_from_parent parent_value =
        ... parent-side computation ...;
        lazy (
          ... child-side computation ...
        )

      let key = Thread_local_storage.new_key ~split_from_parent init

      let get () = Lazy.force (Thread_local_storage.get key)
      ]}

      In this case a part of the computation happens on the child
      thread or domain; in particular, it can access [parent_value]
      concurrently with the parent thread, which may require
      explicit synchronization to avoid data races.
  *)
end

val get : 'a Key.t -> 'a
(** [get k] returns [v] if a value [v] is associated to the key [k] on
    the calling thread's thread-local state. Sets [k]'s value with its
    initialiser and returns it otherwise. *)

val set : 'a Key.t -> 'a -> unit
(** [set k v] updates the calling thread's thread-local state to associate
    the key [k] with value [v]. It overwrites any previous values associated
    to [k], which cannot be restored later. *)
