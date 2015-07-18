
(* CR pchambart:
   constant correspond to Clambda.uconstant
   Allocated_constants correspond to Clambda.structured constant

   It could be replaced by clambda types if they were extended to
   represent every case of Flambdaexport_types

   The constant type does not have a description of the contained
   constant (the option in the Uconst_ref constructor). For rebuilding
   clambda, we may need to find the description: this is usefull on
   floats (and boxed integers) for unboxing.

*)

type constant =
  | Symbol of Symbol.t
  | Int of int
  | Const_pointer of int

module Allocated_constants : sig


  (* PR pchambart: The ['a] type variable is a sad side effect of the
     implementation. In this module it is instanciated with constant
     and Variable.t. I'm may be worth duplicating the type to avoid
     it in the interface *)
  type 'a t =
    | Float of float
    | Int32 of int32
    | Int64 of int64
    | Nativeint of nativeint
    | Float_array of float list
    | String of string
    | Immstring of string
    | Block of Tag.t * 'a list

end

type result = {
  expr : Flambda.t;
  (** The main expression with constant declarations removed and
      accesses replaced by a symbol or an integer. *)
  constant_descr : constant Allocated_constants.t Symbol.Map.t;
  (** The constants that should be emitted. *)
  kind : constant Variable.Map.t;
  (** How to substitute a variable *)
  set_of_closures_map : Flambda.set_of_closures Symbol.Map.t;
  (** Constant closures that should be emitted *)
}

val lift_constants : Flambda.t -> result
