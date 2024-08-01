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

    {b Note:} Thread-local storage is designed for fast, constant-time
    access to a small number of keys, typically declared at the
    top-level of the module. It is not designed to be fast in cases
    where keys are created dynamically.

    @since TODO *)

type 'a t
(** Type of a thread-local storage key. *)

val make : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a t
(** [make f] returns a new key bound to initialiser [f] for accessing
    thread-local variables.

    If [split_from_parent] is not provided, the value for a new
    thread will be computed on-demand by the new thread: the first
    [get] call will call the initializer [f] and store that value.

    {b Warning.} [f] may be called several times if another call
    to [get] occurs during initialization on the same thread. Only
    the 'first' value computed will be used, the other now-useless
    values will be discarded. Your initialization function should
    support this situation, or contain logic to detect this case
    and fail.

    If [split_from_parent] is provided, spawning a thread will
    derive the child value (for this key) from the parent
    value. This computation happens in the parent thread and it
    always happens, regardless of whether the child thread will
    use it.
    If the splitting function is expensive or requires
    child-side computation, consider using ['a Lazy.t key]:

    {[
    let init () = ...

    let split_from_parent parent_value =
      ... parent-side computation ...;
      lazy (
        ... child-side computation ...
      )

    let key = Thread_local_storage.make ~split_from_parent init

    let get () = Lazy.force (Thread_local_storage.get key)
    ]}

    In this case a part of the computation happens on the child
    thread; in particular, it can access [parent_value]
    concurrently with the parent thread, which may require
    explicit synchronization.
*)

val get : 'a t -> 'a
(** [get k] returns [v] if a value [v] is associated to the key [k] on
    the calling thread's thread-local state. Sets [k]'s value with its
    initialiser and returns it otherwise. *)

val set : 'a t -> 'a -> unit
(** [set k v] updates the calling thread's thread-local state to associate
    the key [k] with value [v]. It overwrites any previous values associated
    to [k], which cannot be restored later. *)

val at_exit : (unit -> unit) -> unit
(** Registers, on the calling thread only, a function to run when the
    thread terminates -- either correctly or with a [Thread.Exit] or
    uncaught exception.

    This function is typically in the initialization code for
    thread-local resource, to register a release callback for this
    resource:

    {[
    let temp_file_key = Thread_local_storage.make (fun () ->
      let (file, channel) = Filename.open_temp_file "log" ".out" in
      Thread_local_storage.at_exit (fun () -> close_out_noerr channel);
      (file, channel)
    )
    ]}

    Note that, with this definition, there is no runtime cost for
    threads that do not access [temp_file_key], and that the callback
    is *not* registered if you call [Thead_local_storage.set] before
    the first [get], which lets you override the default generator
    with thread-specific choices which may require a different release
    logic.
*)
