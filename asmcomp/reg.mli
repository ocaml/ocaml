(* Pseudo-registers *)

type t =
  { mutable name: string;
    stamp: int;
    typ: Cmm.machtype_component;
    mutable loc: location;
    mutable interf: t list;
    mutable prefer: (t * int) list;
    mutable degree: int;
    mutable spill_cost: int;
    mutable visited: bool }

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

val dummy: t
val new: Cmm.machtype_component -> t
val newv: Cmm.machtype -> t array
val clone: t -> t
val at_location: Cmm.machtype_component -> location -> t

module Set: Set.S with elt = t
module Map: Map.S with key = t

val add_set_array: Set.t -> t array -> Set.t
val diff_set_array: Set.t -> t array -> Set.t
val inter_set_array: Set.t -> t array -> Set.t
val set_of_array: t array -> Set.t

val reset: unit -> unit
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
