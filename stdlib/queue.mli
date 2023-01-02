(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** First-in first-out queues.

   This module implements queues (FIFOs), with in-place modification.
   See {{!examples} the example section} below.
*)

(** {b Unsynchronized accesses} *)

[@@@alert unsynchronized_access
    "Unsynchronized accesses to queues are a programming error."
]

(**
    Unsynchronized accesses to a queue may lead to an invalid queue state.
    Thus, concurrent accesses to queues must be synchronized (for instance
    with a {!Mutex.t}).
*)

type !'a t
(** The type of queues containing elements of type ['a]. *)


exception Empty
(** Raised when {!Queue.take} or {!Queue.peek} is applied to an empty queue. *)


val create : unit -> 'a t
(** Return a new queue, initially empty. *)

val add : 'a -> 'a t -> unit
(** [add x q] adds the element [x] at the end of the queue [q]. *)

val push : 'a -> 'a t -> unit
(** [push] is a synonym for [add]. *)

val take : 'a t -> 'a
(** [take q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. *)

val take_opt : 'a t -> 'a option
(** [take_opt q] removes and returns the first element in queue [q],
   or returns [None] if the queue is empty.
   @since 4.08 *)

val pop : 'a t -> 'a
(** [pop] is a synonym for [take]. *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q], without removing
   it from the queue, or raises {!Empty} if the queue is empty. *)

val peek_opt : 'a t -> 'a option
(** [peek_opt q] returns the first element in queue [q], without removing
   it from the queue, or returns [None] if the queue is empty.
   @since 4.08 *)

val top : 'a t -> 'a
(** [top] is a synonym for [peek]. *)

val clear : 'a t -> unit
(** Discard all elements from a queue. *)

val copy : 'a t -> 'a t
(** Return a copy of the given queue. *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)

val length : 'a t -> int
(** Return the number of elements in a queue. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The queue itself is unchanged. *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f accu q] is equivalent to [List.fold_left f accu l],
   where [l] is the list of [q]'s elements. The queue remains
   unchanged. *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
   the queue [q2], then clears [q1]. It is equivalent to the
   sequence [iter (fun x -> add x q2) q1; clear q1], but runs
   in constant time. *)

(** {1 Iterators} *)

val to_seq : 'a t -> 'a Seq.t
(** Iterate on the queue, in front-to-back order.
    The behavior is not specified if the queue is modified
    during the iteration.
    @since 4.07 *)

val add_seq : 'a t -> 'a Seq.t -> unit
(** Add the elements from a sequence to the end of the queue.
    @since 4.07 *)

val of_seq : 'a Seq.t -> 'a t
(** Create a queue from a sequence.
    @since 4.07 *)

(** {1:examples Examples}

  {2 Basic Example}

   A basic example:
    {[
    # let q = Queue.create ()
    val q : '_weak1 Queue.t = <abstr>


    # Queue.push 1 q; Queue.push 2 q; Queue.push 3 q
    - : unit = ()

    # Queue.length q
    - : int = 3

    # Queue.pop q
    - : int = 1

    # Queue.pop q
    - : int = 2

    # Queue.pop q
    - : int = 3

    # Queue.pop q
    Exception: Stdlib.Queue.Empty.
    ]}

  {2 Search Through a Graph}

   For a more elaborate example, a classic algorithmic use of queues
   is to implement a BFS (breadth-first search) through a graph.

   {[
     type graph = {
       edges: (int, int list) Hashtbl.t
     }

    (* Search in graph [g] using BFS, starting from node [start].
       It returns the first node that satisfies [p], or [None] if
       no node reachable from [start] satisfies [p].
    *)
    let search_for ~(g:graph) ~(start:int) (p:int -> bool) : int option =
      let to_explore = Queue.create() in
      let explored = Hashtbl.create 16 in

      Queue.push start to_explore;
      let rec loop () =
        if Queue.is_empty to_explore then None
        else
          (* node to explore *)
          let node = Queue.pop to_explore in
          explore_node node

      and explore_node node =
        if not (Hashtbl.mem explored node) then (
          if p node then Some node (* found *)
          else (
            Hashtbl.add explored node ();
            let children =
              Hashtbl.find_opt g.edges node
              |> Option.value ~default:[]
            in
            List.iter (fun child -> Queue.push child to_explore) children;
            loop()
          )
        ) else loop()
      in
      loop()

    (* a sample graph *)
    let my_graph: graph =
      let edges =
        List.to_seq [
          1, [2;3];
          2, [10; 11];
          3, [4;5];
          5, [100];
          11, [0; 20];
        ]
        |> Hashtbl.of_seq
      in {edges}

    # search_for ~g:my_graph ~start:1 (fun x -> x = 30)
    - : int option = None

    # search_for ~g:my_graph ~start:1 (fun x -> x >= 15)
    - : int option = Some 20

    # search_for ~g:my_graph ~start:1 (fun x -> x >= 50)
    - : int option = Some 100
   ]}
   *)
