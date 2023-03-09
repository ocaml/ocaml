(* TEST *)

(* We present our priority queues as a functor
   parametrized on the comparison function. *)
module Heap (Elem : Map.OrderedType) : sig
  type t
  val create : unit -> t
  val add : t -> Elem.t -> unit
  val pop_min : t -> Elem.t option
end = struct

  (* Our priority queues are implemented using the standard "min heap"
     data structure, a dynamic array representing a binary tree. *)
  type t = Elem.t Dynarray.t
  let create = Dynarray.create

 (* The node of index [i] has as children the nodes of index [2 * i + 1]
    and [2 * i + 2] -- if they are valid indices in the dynarray. *)
  let left_child i = 2 * i + 1
  let right_child i = 2 * i + 2
  let parent_node i = (i - 1) / 2

  (* We use indexing operators for convenient notations. *)
  let ( .!() ) = Dynarray.get
  let ( .!()<- ) = Dynarray.set

  (* Auxiliary functions to compare and swap two elements
     in the dynamic array. *)
  let order h i j =
    Elem.compare h.!(i) h.!(j)

  let swap h i j =
    let v = h.!(i) in
    h.!(i) <- h.!(j);
    h.!(j) <- v

  (* We say that a heap respects the "heap ordering" if the value of
     each node is smaller than the value of its children. The
     algorithm manipulates arrays that respect the heap algorithm,
     except for one node whose value may be too small or too large.

     The auxiliary functions [heap_up] and [heap_down] take
     such a misplaced value, and move it "up" (respectively: "down")
     the tree by permuting it with its parent value (respectively:
     a children's value) until the heap ordering is restored. *)

  let rec heap_up h i =
    if i = 0 then () else
    let parent = parent_node i in
    if order h i parent < 0 then
      (swap h i parent; heap_up h parent)

  and heap_down h ~len i =
    let left, right = left_child i, right_child i in
    if left >= len then () (* no child, stop *) else
    let smallest =
      if right >= len then left (* no right child *) else
      if order h left right < 0 then left else right
    in
    if order h i smallest > 0 then
      (swap h i smallest; heap_down h ~len smallest)

  let add h s =
    let i = Dynarray.length h in
    Dynarray.add_last h s;
    heap_up h i

  let pop_min h =
    if Dynarray.is_empty h then None
    else begin
      (* Standard trick: swap the 'best' value at index 0
         with the last value of the array. *)
      let last = Dynarray.length h - 1 in
      swap h 0 last;
      (* At this point [pop_last] returns the 'best' value,
         and leaves a heap with one misplaced element at position 0. *)
      let best = Dynarray.pop_last h in
      (* Restore the heap ordering -- does nothing if the heap is empty. *)
      heap_down h ~len:last 0;
      Some best
    end
end

let heap_sort (type a) cmp li =
  let module Heap = Heap(struct type t = a let compare = cmp end) in
  let heap = Heap.create () in
  List.iter (Heap.add heap) li;
  List.map (fun _ -> Heap.pop_min heap |> Option.get) li

let () =
  let rev cmp x y = cmp y x in
  assert (heap_sort compare [3; 1; 2; 7; 2; 5] = [1; 2; 2; 3; 5; 7]);
  assert (heap_sort (rev compare) [3; 1; 2; 7; 2; 5] = [7; 5; 3; 2; 2; 1]);
  for i = 1 to 1_000 do
    let li = List.init 10 (fun _ -> Random.int 10) in
    assert (heap_sort compare li = List.sort compare li);
    assert (heap_sort (rev compare) li = List.sort (rev compare) li);
  done
