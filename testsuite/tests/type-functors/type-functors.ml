(* TEST
   * expect
*)

module type S = sig
  type t

  type 'a u

  val create : unit -> t

  val create_s : 'a -> 'a u

  module T : sig
    type t
  end
end;;

module type T = sig
  type t
end;;

module type F = sig
  module F : functor (M : S) -> T;;
end

[%%expect{|
module type S =
  sig
    type t
    type 'a u
    val create : unit -> t
    val create_s : 'a -> 'a u
    module T : sig type t end
  end
module type T = sig type t end
module type F = sig module F : functor (M : S) -> T end
|}]

(* Value bindings. *)

let e () : {M : S} -> int = assert false;;

let f () : {M : S} -> M.t = assert false;;

let g () : {M : S} -> 'a -> 'a M.u = assert false;;

let h () : {M : S} -> M.T.t = assert false;;

let i () : {M : S} -> {F : F} -> F.F(M).t = assert false;;

let e_unify b (e' : {N : S} -> int) = if b then e () else e';;

let f_unify b (f' : {N : S} -> N.t) = if b then f () else f';;

let g_unify b (g' : {N : S} -> 'a -> 'a N.u) = if b then g () else g';;

let h_unify b (h' : {N : S} -> N.T.t) = if b then h () else h';;

let i_unify b (i' : {N : S} -> {F' : F} -> F'.F(N).t) =
  if b then i () else i';;

[%%expect{|
val e : unit -> {M : S} -> int = <fun>
val f : unit -> {M : S} -> M.t = <fun>
val g : unit -> {M : S} -> 'a -> 'a M.u = <fun>
val h : unit -> {M : S} -> M.T.t = <fun>
val i : unit -> {M : S} -> {F : F} -> F.F(M).t = <fun>
val e_unify : bool -> ({N : S} -> int) -> {M : S} -> int = <fun>
val f_unify : bool -> ({N : S} -> N.t) -> {M : S} -> M.t = <fun>
val g_unify : bool -> ({N : S} -> 'a -> 'a N.u) -> {M : S} -> 'a -> 'a M.u =
  <fun>
val h_unify : bool -> ({N : S} -> N.T.t) -> {M : S} -> M.T.t = <fun>
val i_unify :
  bool ->
  ({N : S} -> {F' : F} -> F'.F(N).t) -> {M : S} -> {F : F} -> F.F(M).t =
  <fun>
|}]

(* Value coercion. *)

let f_unify b1 b2 (f' : {M : S with type t = bool} -> M.t)
    (f'' : {M : S with type t = bool} -> bool) =
  if b1 then (f () :> {M : S with type t = bool} -> M.t)
  else if b2 then f' else f'';;

module type T' = sig
  include T

  type u
end;;

let increase_functor (f : {M : T} -> M.t) : {M : T'} -> M.t =
  (f :> {M : T'} -> M.t);;

[%%expect{|
val f_unify :
  bool ->
  bool ->
  ({M : S with type t = bool} -> M.t) ->
  ({M : S with type t = bool} -> bool) -> {M : S with type t = bool} -> M.t =
  <fun>
module type T' = sig type t type u end
val increase_functor : ({M : T} -> M.t) -> {M : T'} -> M.t = <fun>
|}]

(* Type definitions. *)

type independent = {M : S} -> unit;;

type plain = {M : S} -> M.t;;

type 'a type_variable = {M : S} -> 'a;;

type nested = {M : S} -> M.t -> {M : S} -> M.t;;

type constructor_arg = A of ({M : S} -> M.t);;

type 'a constructor_arg_param = B of ({M : S} -> 'a -> 'a M.u);;

type _ constructor_arg_gadt =
  | C : ({M : S} -> 'b -> ('a -> 'b) M.u) -> 'a constructor_arg_gadt;;

type record = {field: {M : S} -> M.t};;

type poly_record = {poly_field: 'a. {M : S} -> 'a -> 'a M.u};;

type fixed_variant = [`A of ({M : S} -> M.t)];;

type ('a, 'b) open_variant = [> `B of ({M : S} -> 'b -> 'b M.u)] as 'a;;

type 'a class_type =
  < a: {M : S} -> unit
  ; b: {M : S} -> M.t
  ; c: {M : S} -> 'a -> 'a M.u >;;

[%%expect{|
type independent = {M : S} -> unit
type plain = {M : S} -> M.t
type 'a type_variable = {M : S} -> 'a
type nested = {M/1 : S} -> M/1.t -> {M/2 : S} -> M/2.t
type constructor_arg = A of ({M : S} -> M.t)
type 'a constructor_arg_param = B of ({M : S} -> 'a -> 'a M.u)
type _ constructor_arg_gadt =
    C : ({M : S} -> 'b -> ('a -> 'b) M.u) -> 'a constructor_arg_gadt
type record = { field : {M : S} -> M.t; }
type poly_record = { poly_field : 'a. {M : S} -> 'a -> 'a M.u; }
type fixed_variant = [ `A of {M : S} -> M.t ]
type ('a, 'b) open_variant = 'a
  constraint 'a = [> `B of {M : S} -> 'b -> 'b M.u ]
type 'a class_type =
    < a : {M : S} -> unit; b : {M : S} -> M.t; c : {M : S} -> 'a -> 'a M.u >
|}]

(* Invalid type definitions. *)

type 'a open_variant_rhs = {M : S} -> ([> `A of M.t] as 'a);;
[%%expect{|
Line 1, characters 0-59:
1 | type 'a open_variant_rhs = {M : S} -> ([> `A of M.t] as 'a);;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type variable is unbound in this type declaration.
In type {M : S} -> ([> `A of M.t ] as 'a) the variable 'a is unbound
|}]

(* Unification with types. *)

let independent (x : {M : S} -> unit) : independent = x;;

let independent2 (x : independent) : {M : S} -> unit = x;;

let plain (x : {M : S} -> M.t) : plain = x;;

let plain2 (x : plain) : {M : S} -> M.t = x;;

let type_variable (x : {M : S} -> 'a) : 'a type_variable = x;;

let type_variable2 (x : 'a type_variable ) : {M : S} -> 'a = x;;

let nested (x : nested) : {M : S} -> M.t -> {M : S} -> M.t = x;;

let constructor_arg (x : {M : S} -> M.t) : constructor_arg = A x;;

let constructor_arg2 (x : constructor_arg) : {M : S} -> M.t =
  match x with
  | A x -> x;;

let constructor_arg_param (x : {M : S} -> 'a -> 'a M.u)
  : 'a constructor_arg_param =
  B x;;

let constructor_arg_param2 (x : 'a constructor_arg_param)
  : {M : S} -> 'a -> 'a M.u =
  match x with
  | B x -> x;;

let constructor_arg_gadt (x : {M : S} -> _ -> _ M.u) : _ constructor_arg_gadt =
  C x;;

let record (x : {M : S} -> M.t) : record = {field= x};;

let record2 (x : record) : {M : S} -> M.t = x.field;;

let poly_record (x : poly_record) : {M : S} -> 'a -> 'a M.u = x.poly_field;;

let fixed_variant (x : {M : S} -> M.t) : fixed_variant = `A x;;

let fixed_variant2 (x : fixed_variant) : {M : S} -> M.t =
  match x with
  | `A x -> x;;

let open_variant b (x : {M : S} -> 'b -> 'b M.u) : _ open_variant =
  if b then `B x else `C 15;;

let class_type (a : {M : S} -> unit) (b : {M : S} -> M.t)
    (c : {M : S} -> 'a -> 'a M.u)
  : 'a class_type =
  object
    method a = a
    method b = b
    method c = c
  end;;

let class_type_a (x : 'a class_type) : {M : S} -> unit = x#a;;

let class_type_b (x : 'a class_type) : {M : S} -> M.t = x#b;;

let class_type_c (x : 'a class_type) : {M : S} -> 'a -> 'a M.u = x#c;;

[%%expect{|
val independent : ({M : S} -> unit) -> independent = <fun>
val independent2 : independent -> {M : S} -> unit = <fun>
val plain : ({M : S} -> M.t) -> plain = <fun>
val plain2 : plain -> {M : S} -> M.t = <fun>
val type_variable : ({M : S} -> 'a) -> 'a type_variable = <fun>
val type_variable2 : 'a type_variable -> {M : S} -> 'a = <fun>
val nested : nested -> {M/1 : S} -> M/1.t -> {M/2 : S} -> M/2.t = <fun>
val constructor_arg : ({M : S} -> M.t) -> constructor_arg = <fun>
val constructor_arg2 : constructor_arg -> {M : S} -> M.t = <fun>
val constructor_arg_param :
  ({M : S} -> 'a -> 'a M.u) -> 'a constructor_arg_param = <fun>
val constructor_arg_param2 :
  'a constructor_arg_param -> {M : S} -> 'a -> 'a M.u = <fun>
val constructor_arg_gadt :
  ({M : S} -> 'a -> ('b -> 'a) M.u) -> 'b constructor_arg_gadt = <fun>
val record : ({M : S} -> M.t) -> record = <fun>
val record2 : record -> {M : S} -> M.t = <fun>
val poly_record : poly_record -> {M : S} -> 'a -> 'a M.u = <fun>
val fixed_variant : ({M : S} -> M.t) -> fixed_variant = <fun>
val fixed_variant2 : fixed_variant -> {M : S} -> M.t = <fun>
val open_variant :
  bool ->
  ({M : S} -> 'b -> 'b M.u) ->
  ([> `B of {M : S} -> 'b -> 'b M.u | `C of int ], 'b) open_variant = <fun>
val class_type :
  ({M : S} -> unit) ->
  ({M : S} -> M.t) -> ({M : S} -> 'a -> 'a M.u) -> 'a class_type = <fun>
val class_type_a : 'a class_type -> {M : S} -> unit = <fun>
val class_type_b : 'a class_type -> {M : S} -> M.t = <fun>
val class_type_c : 'a class_type -> {M : S} -> 'a -> 'a M.u = <fun>
|}]

let constructor_arg_gadt2 (x : _ constructor_arg_gadt)
  : {M : S} -> _ -> _ M.u =
  match x with
  | C x -> x;;
[%%expect{|
Line 4, characters 11-12:
4 |   | C x -> x;;
               ^
Error: This expression has type {M : S} -> $C_'b -> ('a -> $C_'b) M.u
       but an expression was expected of type {M : S} -> 'b -> 'c M.u
       The type constructor $C_'b would escape its scope
|}]

(* Types with paramters can be recursively applied. *)

type recursed = unit type_variable type_variable type_variable;;

let recursed b (x : recursed) (y : {A : S} -> {B : S} -> {C : S} -> unit) =
  if b then x else y;;

[%%expect{|
type recursed = unit type_variable type_variable type_variable
val recursed :
  bool -> recursed -> ({A : S} -> {B : S} -> {C : S} -> unit) -> recursed =
  <fun>
|}]

let recursed_bad b (x : recursed) (y : {A : S} -> {B : S} -> unit) =
  if b then x else y;;
[%%expect{|
Line 2, characters 19-20:
2 |   if b then x else y;;
                       ^
Error: This expression has type {A : S} -> {B : S} -> unit
       but an expression was expected of type
         recursed = {M : S} -> unit type_variable type_variable
       Type {B : S} -> unit is not compatible with type
         unit type_variable type_variable = {M : S} -> unit type_variable
       Type unit is not compatible with type
         unit type_variable = {M : S} -> unit
|}]

(* Module names are not interchanged during unification. *)

let scoping b (x : {A : S} -> {B : S} -> {C : S} -> A.t -> B.t -> C.t)
    (y : {M : S} -> {M : S} -> {M : S} -> M.t -> M.t -> M.t) =
  if b then x else y;;
[%%expect{|
Line 3, characters 19-20:
3 |   if b then x else y;;
                       ^
Error: This expression has type
         {M/1 : S} -> {M/2 : S} -> {M/3 : S} -> M/3.t -> M/3.t -> M/3.t
       but an expression was expected of type
         {A : S} -> {B : S} -> {C : S} -> A.t -> B.t -> C.t
       Type M.t is not compatible with type A.t
|}]

type ('a, 'b) scoping_fn = {M : S} -> M.t -> 'a M.u -> 'b;;

type 'a recursive_scoping =
  ('a, ('a, ('a, unit) scoping_fn) scoping_fn) scoping_fn;;

[%%expect{|
type ('a, 'b) scoping_fn = {M : S} -> M.t -> 'a M.u -> 'b
type 'a recursive_scoping =
    ('a, ('a, ('a, unit) scoping_fn) scoping_fn) scoping_fn
|}]

let recursive_scoping b (x : 'a recursive_scoping)
    (y : {M : S} -> M.t -> 'a M.u -> {A : S} -> M.t -> 'a M.u -> {A : S} -> M.t
         -> 'a M.u -> unit) =
  if b then x else y;;
[%%expect{|
Line 4, characters 19-20:
4 |   if b then x else y;;
                       ^
Error: This expression has type
         {M : S} ->
         M.t ->
         'a M.u ->
         {A/1 : S} -> M.t -> 'a M.u -> {A/2 : S} -> M.t -> 'a M.u -> unit
       but an expression was expected of type
         'a recursive_scoping =
           {M : S} -> M.t -> 'a M.u -> ('a, ('a, unit) scoping_fn) scoping_fn
       Type
         {A/1 : S} ->
         M/1.t -> 'a M/1.u -> {A/2 : S} -> M/1.t -> 'a M/1.u -> unit
       is not compatible with type
         ('a, ('a, unit) scoping_fn) scoping_fn =
           {M/2 : S} -> M/2.t -> 'a M/2.u -> ('a, unit) scoping_fn
       Type M/1.t is not compatible with type M/2.t
|}]

(* Scope escape. *)

let scope_escape (f : ({M : S} -> 'a) -> 'a) (x : {M : S} -> M.t) = f x;;
[%%expect{|
Line 1, characters 70-71:
1 | let scope_escape (f : ({M : S} -> 'a) -> 'a) (x : {M : S} -> M.t) = f x;;
                                                                          ^
Error: This expression has type {M : S} -> M.t
       but an expression was expected of type {M : S} -> 'a
       The type constructor M.t would escape its scope
|}]

let scope_escape2 () : 'a -> {M : S} -> (M.t as 'a) = assert false;;
[%%expect{|
Line 1, characters 41-50:
1 | let scope_escape2 () : 'a -> {M : S} -> (M.t as 'a) = assert false;;
                                             ^^^^^^^^^
Error: This alias is bound to type M.t but is used as an instance of type 'a
       The type constructor M.t would escape its scope
|}]

let scope_escape3 () : 'a -> {M : S} -> ([`A of M.t] as 'a) = assert false;;
[%%expect{|
Line 1, characters 41-58:
1 | let scope_escape3 () : 'a -> {M : S} -> ([`A of M.t] as 'a) = assert false;;
                                             ^^^^^^^^^^^^^^^^^
Error: This alias is bound to type [ `A of M.t ]
       but is used as an instance of type 'a
       The type constructor M.t would escape its scope
|}]

let scope_escape4 (f : {M : S} -> M.t) : {N : S} -> 'a = f;;
[%%expect{|
Line 1, characters 57-58:
1 | let scope_escape4 (f : {M : S} -> M.t) : {N : S} -> 'a = f;;
                                                             ^
Error: This expression has type {M : S} -> M.t
       but an expression was expected of type {N : S} -> 'a
       The type constructor M.t would escape its scope
|}]

(* Type names are kept distinct. *)

let distinct_types b (f : {M : T'} -> M.t) (g : {M : T'} -> M.u) =
  if b then f else g;;
[%%expect{|
Line 2, characters 19-20:
2 |   if b then f else g;;
                       ^
Error: This expression has type {M : T'} -> M.u
       but an expression was expected of type {M : T'} -> M.t
       Type M/1.u is not compatible with type M/2.t
|}]

(* Substitutions are opened if there would be a variable escape. *)

type ('a, 'b) t = 'a -> {M : S with type t = 'b} -> (M.t as 'a);;
[%%expect{|
type ('a, 'b) t = 'a -> {M : S with type t = 'a} -> 'a constraint 'b = 'a
|}]

(* Bound module names are not mixed when there is a unification error part-way
   through unifying a type.
*)
let f (x : ({X : S} -> X.t * bool)) : {Y : S} -> Y.t * unit = x;;
[%%expect{|
Line 1, characters 62-63:
1 | let f (x : ({X : S} -> X.t * bool)) : {Y : S} -> Y.t * unit = x;;
                                                                  ^
Error: This expression has type {X : S} -> X.t * bool
       but an expression was expected of type {Y : S} -> Y.t * unit
       Type bool is not compatible with type unit
|}]
