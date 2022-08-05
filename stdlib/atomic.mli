(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Atomic references.

    A basic use case is to have global counters that are updated in a
    thread-safe way:

    {[
    (* our counter *)
    let count_bytes_read = Atomic.make 0

    (* prepare a sample file *)
    let () =
      let oc = open_out "/tmp/example_data" in
      for i=1 to 100_000 do output_char oc 'x' done;
      close_out oc

    (* just read from file, discard content but count bytes.
       This is pretty useless and only used to show this. *)
    let read_file () =
      let ic = open_in "/tmp/example_data" in
      let buf = Bytes.create 1024 in
      let rec read_next_chunk () =
        let n = input ic buf 0 1024 in
        Thread.yield();
        if n> 0 then (
          (* count_bytes_read += n, atomically *)
          ignore (Atomic.fetch_and_add count_bytes_read n : int);
          read_next_chunk()
        )
      in
      read_next_chunk()

    (* run multiple threads (or domains) that update the counter *)
    # let () =
      let threads = Array.init 8 (fun _ -> Thread.create read_file ()) in
      Array.iter Thread.join threads;
      Printf.printf "read %d bytes\n" (Atomic.get count_bytes_read)
    - : unit = ()
    read 800000 bytes

    ]}

    Another example is a basic
    {{: https://en.wikipedia.org/wiki/Treiber_stack} Treiber stack}
      (a thread-safe stack):
    {[
    type 'a stack = 'a list Atomic.t

    let rec push (stack: _ stack) elt : unit =
      let cur = Atomic.get stack in
      let success = Atomic.compare_and_set stack cur (elt :: cur) in
      if not success then
        push stack elt

    let rec pop (stack: _ stack) : _ option =
      let cur = Atomic.get stack in
      match cur with
      | [] -> None
      | x :: tail ->
        let success = Atomic.compare_and_set stack cur tail in
        if success then Some x
        else pop stack

    # let st = Atomic.make []
    # push st 1
    - : unit = ()
    # push st 2
    - : unit = ()
    # pop st
    - : int option = Some 2
    # pop st
    - : int option = Some 1
    # pop st
    - : int option = None
    ]}

    @since 4.12
*)

(** An atomic (mutable) reference to a value of type ['a]. *)
type !'a t

(** Create an atomic reference. *)
val make : 'a -> 'a t

(** Get the current value of the atomic reference. *)
val get : 'a t -> 'a

(** Set a new value for the atomic reference. *)
val set : 'a t -> 'a -> unit

(** Set a new value for the atomic reference, and return the current value. *)
val exchange : 'a t -> 'a -> 'a

(** [compare_and_set r seen v] sets the new value of [r] to [v] only
    if its current value is physically equal to [seen] -- the
    comparison and the set occur atomically. Returns [true] if the
    comparison succeeded (so the set happened) and [false]
    otherwise. *)
val compare_and_set : 'a t -> 'a -> 'a -> bool

(** [fetch_and_add r n] atomically increments the value of [r] by [n],
    and returns the current value (before the increment). *)
val fetch_and_add : int t -> int -> int

(** [incr r] atomically increments the value of [r] by [1]. *)
val incr : int t -> unit

(** [decr r] atomically decrements the value of [r] by [1]. *)
val decr : int t -> unit
