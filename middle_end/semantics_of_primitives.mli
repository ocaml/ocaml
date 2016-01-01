(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Library General Public License version 2.1, with the         *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: comment is out of date *)
(** Description of the semantics of primitives, to be used for optimization
    purposes.

    "No effects" means that the primitive does not change the observable state
    of the world.  For example, it must not write to any mutable storage,
    call arbitrary external functions or change control flow (e.g. by raising
    an exception).
 
    It is assumed in the compiler that expressions with no effects, whose
    results are not used, may be eliminated.
 
    (Exceptions arising from allocation points, for example "out of memory" or
    exceptions propagated from finalizers or signal handlers, are treated as
    "effects out of the ether".  The corresponding expressions are deemed to
    have no effects.  The same goes for floating point operations that may
    cause hardware traps on some platforms.)
 
    "No coeffects" means that the primitive does not observe the effects (in
    the sense described above) of other expressions.  For example, it must not
    read from any mutable storage or call arbitrary external functions.
 
    It is assumed in the compiler that, subject to data dependencies,
    expressions with neither effects nor coeffects may be reordered with
    respect to other expressions.
*)

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

(** Describe the semantics of a primitive.  This does not take into account of
    the (non-)(co)effectfulness of the arguments in a primitive application.
    To determine whether such an application is (co)effectful, the arguments
    must also be analysed. *)
val for_primitive
   : Lambda.primitive
  -> effects * coeffects
