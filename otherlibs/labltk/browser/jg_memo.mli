(* $Id$ *)

val fast : fun:('a -> 'b) -> 'a -> 'b
(* "fast" memoizer: uses a List.assq like function      *)
(* Good for a smallish number of keys, phisically equal *)
