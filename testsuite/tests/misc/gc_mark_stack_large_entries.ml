(* TEST *)

(* Regression test for #15327. Compressing the mark stack leaves large
   entries (spans of more than [Sys.word_size] fields) in place. A mark
   stack which is full of large entries must therefore grow, even when
   it is over the bound set by [mark_stack_prune_factor]. *)

type node = Node of node array [@@unboxed]

(* Every field of a node points to the next node in the chain. Marking
   a node scans only some of its fields before it reaches the next
   node, and leaves the rest on the mark stack as a large entry. So
   marking the chain puts [length] large entries on the mark stack.
   [length] must exceed the initial mark stack size (4096 entries).
   [width] must exceed the number of fields scanned (at most 448, from
   the size of the marker's prefetch buffer) by more than
   [Sys.word_size]. *)
let length = 5000
let width = 600

let rec chain n next =
  if n = 0 then next else chain (n - 1) (Node (Array.make width next))

let rec chain_length acc (Node a) =
  if Array.length a = 0 then acc else chain_length (acc + 1) a.(0)

let () =
  (* Make the bound zero, so that the mark stack is always over it. *)
  Gc.Tweak.set "mark_stack_prune_factor" max_int;
  let head = chain length (Node [||]) in
  Gc.full_major ();
  assert (chain_length 0 head = length);
  print_endline "OK"
