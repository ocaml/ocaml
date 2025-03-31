(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Kate Deplaix                              *)
(*                                                                        *)
(*   Copyright 2025 Kate Deplaix                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions defined on the low-level representations of values.

    @since 5.4 *)

(** {1 Physical comparison} *)

external phys_equal : 'a -> 'a -> bool = "%eq"
(** [phys_equal e1 e2] tests for physical equality of [e1] and [e2].
    On mutable types such as references, arrays, byte sequences, records with
    mutable fields and objects with mutable instance variables,
    [phys_equal e1 e2] is true if and only if physical modification of [e1]
    also affects [e2].
    On non-mutable types, the behavior of [phys_equal] is
    implementation-dependent; however, it is guaranteed that
    [phys_equal e1 e2] implies [compare e1 e2 = 0]. *)

(** {1 Polymorphic comparison} *)

external equal : 'a -> 'a -> bool = "%equal"
(** [equal e1 e2] tests for structural equality of [e1] and [e2].
    Mutable structures (e.g. references and arrays) are equal
    if and only if their current contents are structurally equal,
    even if the two mutable objects are not the same physical object.
    Equality between functional values raises [Invalid_argument].
    Equality between cyclic data structures may not terminate. *)

external compare : 'a -> 'a -> int = "%compare"
(** [compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y], and a positive integer
    if [x] is greater than [y].  The ordering implemented by [compare]
    is compatible with the comparison predicates {!Stdlib.( = )},
    {!Stdlib.( < )} and {!Stdlib.( > )}, as well as the [equal] function
    defined above,  with one difference on the treatment of the float value
    {!Stdlib.nan}.  Namely, the comparison predicates treat [nan]
    as different from any other float value, including itself;
    while [repr] treats [nan] as equal to itself and less than any
    other float value.  This treatment of [nan] ensures that [compare]
    defines a total ordering relation.

    [compare] applied to functional values may raise [Invalid_argument].
    [compare] applied to cyclic structures may not terminate.

    The [compare] function can be used as the comparison function
    required by the {!Set.Make} and {!Map.Make} functors, as well as
    the {!List.sort} and {!Array.sort} functions. *)

val min : 'a -> 'a -> 'a
(** Return the smaller of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value {!Stdlib.nan}. *)

val max : 'a -> 'a -> 'a
(** Return the greater of the two arguments.
    The result is unspecified if one of the arguments contains
    the float value {!Stdlib.nan}. *)
