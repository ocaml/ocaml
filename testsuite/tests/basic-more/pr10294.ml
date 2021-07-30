(* TEST *)

type import_error = Node of string
type export_error = Variant of string * string

exception Import of import_error
exception Export of export_error
(* Pattern-matching analysis and compilation considers that two
   exceptions constructors may be equal (one may be a rebinding of
   the other) as long as they have the same arity, as is the case
   here.

   The result of splitting on these two exception constructors is what
   we call an "incoherent row", a pattern matrix whose rows have
   incompatible types (one matching on [import_error], the other on
   [export_error]).

   In the case of the code below, the incoherent row is as follows:

   (Node _)
   (Variant (_, _))

   Note that the two constructors [Node] and [Variant] have different
   arities, but the same tag (0).

   In bug #10924, this causes an assertion-failure in the
   pattern-matching compiler, because a matrix-decomposition
   computation in Default_environment ends up considering that Node
   and Variant are equal, creating a sub-matrix with one wildcard
   pattern in the first row, and two in the second.

   This is fixed by comparing constructors by more than their tags
   (which is insufficient for incoherent rows).
*)
let f = function
  | Import (Node _) ->
      1
  | Export (Variant (_, _)) ->
      2
  | _ ->
      3

let () =
  assert (f (Import (Node "foo")) = 1);
  assert (f (Export (Variant ("foo", "bar"))) = 2);
