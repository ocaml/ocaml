(* Find type from a location *)

exception Type of Types.type_expr

let current_loc = ref Location.none

module ForIterator = struct
    open Asttypes
    open Types
    open Typedtree

    include TypedtreeIter.DefaultIteratorArgument

    let enter_expression exp =
      if exp.exp_loc = !current_loc then raise (Type exp.exp_type)
end

module Iterator = TypedtreeIter.MakeIterator(ForIterator)
