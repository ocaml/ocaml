(* TEST
   * expect
*)

module type TYPE = sig
  type t
end;;

module type S = sig
  type t
  type 'a t1
  type 'a t1_alias = 'a list
  type ('a, _) t2_inner_alias = 'a t1
  type ('a, _) t2_outer_alias = 'a option
  module type S
  module type S_alias = S
  module type S_outer_alias = TYPE
  module type Sig = sig
    type t
  end
  module type Sig_alias = Sig
end;;

[%%expect{|
module type TYPE = sig type t end
module type S =
  sig
    type t
    type 'a t1
    type 'a t1_alias = 'a list
    type ('a, _) t2_inner_alias = 'a t1
    type ('a, _) t2_outer_alias = 'a option
    module type S
    module type S_alias = S
    module type S_outer_alias = TYPE
    module type Sig = sig type t end
    module type Sig_alias = Sig
  end
|}]

(* NOTE: The _fail suffixes below indicate that an escape error should be
   thrown. All other definitions (except those named [f]) should also fail, and
   the true test for these is that the functor identifier does not appear in
   the error message.
*)

type _ type_escape = A : ({M : S} -> M.t) type_escape;;

[%%expect{|
type _ type_escape = A : ({M : S} -> M.t) type_escape
|}]

let type_escape_fail (type a) (x : a) (y : ({M : S} -> a) type_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 7-8:
3 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type ({M : S} -> M.t) type_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) type_escape
       The type constructor M.t would escape its scope
|}]

let f (type a) (x : a) (y : a type_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type_escape -> 'a = <fun>
|}]

type _ type1_escape = A : ({M : S} -> bool M.t1) type1_escape;;

[%%expect{|
type _ type1_escape = A : ({M : S} -> bool M.t1) type1_escape
|}]

let type1_escape_fail (type a) (x : a) (y : ({M : S} -> a) type1_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 7-8:
3 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> bool M.t1) type1_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) type1_escape
       The type constructor M.t1 would escape its scope
|}]

let type1_escape (type a) (x : a) (y : ({M : S} -> a M.t1) type1_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 16-18:
3 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type a
|}]

let f (type a) (x : a) (y : a type1_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type1_escape -> 'a = <fun>
|}]

type _ type1_alias = A : ({M : S} -> bool M.t1_alias) type1_alias;;

[%%expect{|
type _ type1_alias = A : ({M : S} -> bool M.t1_alias) type1_alias
|}]

let type1_alias_expand (type a) (x : a) (y : ({M : S} -> a) type1_alias) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 16-18:
3 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = bool list
|}]

let type1_alias_escape (type a) (x : a)
    (y : ({M : S} -> a M.t1_alias) type1_alias) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 16-18:
4 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = bool
|}]

let f (type a) (x : a) (y : a type1_alias) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type1_alias -> 'a = <fun>
|}]

type _ type2_inner_alias =
  A : ({M : S} -> (unit, bool) M.t2_inner_alias) type2_inner_alias;;

[%%expect{|
type _ type2_inner_alias = A : ({M : S} -> unit M.t1) type2_inner_alias
|}]

let type2_inner_alias_escape_fail (type a) (x : a)
    (y : ({M : S} -> a) type2_inner_alias) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 7-8:
4 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> unit M.t1) type2_inner_alias
       but a pattern was expected which matches values of type
         ({M : S} -> a) type2_inner_alias
       The type constructor M.t1 would escape its scope
|}]

let f (type a) (x : a) (y : a type2_inner_alias) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type2_inner_alias -> 'a = <fun>
|}]

type _ type2_outer_alias =
  A : ({M : S} -> (unit, bool) M.t2_outer_alias) type2_outer_alias;;

[%%expect{|
type _ type2_outer_alias = A : ({M : S} -> unit option) type2_outer_alias
|}]

let type2_outer_alias_escape (type a) (x : a)
    (y : ({M : S} -> a) type2_outer_alias) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 16-18:
4 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = unit option
|}]

let f (type a) (x : a) (y : a type2_outer_alias) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type2_outer_alias -> 'a = <fun>
|}]

type _ type2_outer_alias_expand =
  A : ({M : S} -> (int, M.t) M.t2_outer_alias) type2_outer_alias_expand;;

[%%expect{|
type _ type2_outer_alias_expand =
    A : ({M : S} -> int option) type2_outer_alias_expand
|}]

let type2_outer_alias_expand_escape (type a) (x : a)
    (y : ({M : S} -> a) type2_outer_alias_expand) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 16-18:
4 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = int option
|}]

let f (type a) (x : a) (y : a type2_outer_alias_expand) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type2_outer_alias_expand -> 'a = <fun>
|}]

type _ type2_outer_alias_param_escape =
  A : ({M : S} -> (M.t, bool) M.t2_outer_alias) type2_outer_alias_param_escape;;

[%%expect{|
type _ type2_outer_alias_param_escape =
    A : ({M : S} -> M.t option) type2_outer_alias_param_escape
|}]

let type2_outer_alias_param_escape_fail (type a) (x : a)
    (y : ({M : S} -> a) type2_outer_alias_param_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 7-8:
4 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> M.t option) type2_outer_alias_param_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) type2_outer_alias_param_escape
       The type constructor M.t would escape its scope
|}]

let f (type a) (x : a) (y : a type2_outer_alias_param_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a type2_outer_alias_param_escape -> 'a = <fun>
|}]

type _ mty_escape = A : ({M : S} -> (module M.S)) mty_escape;;

[%%expect{|
type _ mty_escape = A : ({M : S} -> (module M.S)) mty_escape
|}]

let mty_escape_fail (type a) (x : a) (y : ({M : S} -> a) mty_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 7-8:
3 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> (module M.S)) mty_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) mty_escape
       The module type M.S would escape its scope
|}]

let f (type a) (x : a) (y : a mty_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a mty_escape -> 'a = <fun>
|}]

type _ mty_alias_escape =
  A : ({M : S} -> (module M.S_alias)) mty_alias_escape;;

[%%expect{|
type _ mty_alias_escape =
    A : ({M : S} -> (module M.S_alias)) mty_alias_escape
|}]

let mty_alias_escape_fail (type a) (x : a)
    (y : ({M : S} -> a) mty_alias_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 7-8:
4 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> (module M.S_alias)) mty_alias_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) mty_alias_escape
       The module type M.S would escape its scope
|}]

let f (type a) (x : a) (y : a mty_alias_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a mty_alias_escape -> 'a = <fun>
|}]

type _ mty_alias_expand =
  A : ({M : S} -> (module M.S_outer_alias)) mty_alias_expand;;

[%%expect{|
type _ mty_alias_expand =
    A : ({M : S} -> (module M.S_outer_alias)) mty_alias_expand
|}]

let mty_alias_expand (type a) (x : a) (y : ({M : S} -> a) mty_alias_expand) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 16-18:
3 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = (module TYPE)
|}]

let f (type a) (x : a) (y : a mty_alias_expand) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a mty_alias_expand -> 'a = <fun>
|}]

type _ functor_escape = A : ({M : S} -> {N : M.Sig} -> N.t) functor_escape;;

[%%expect{|
type _ functor_escape = A : ({M : S} -> {N : M.Sig} -> N.t) functor_escape
|}]

let functor_escape_fail (type a) (x : a) (y : ({M : S} -> a) functor_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 3, characters 7-8:
3 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> {N : M.Sig} -> N.t) functor_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) functor_escape
       The module type M.Sig would escape its scope
|}]

let f (type a) (x : a) (y : a functor_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a functor_escape -> 'a = <fun>
|}]

type _ functor_alias_escape =
  A : ({M : S} -> {N : M.Sig_alias} -> N.t) functor_alias_escape;;

[%%expect{|
type _ functor_alias_escape =
    A : ({M : S} -> {N : M.Sig_alias} -> N.t) functor_alias_escape
|}]

let functor_alias_escape_fail (type a) (x : a)
    (y : ({M : S} -> a) functor_alias_escape) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 7-8:
4 |   | x, A -> x = 15;;
           ^
Error: This pattern matches values of type
         ({M : S} -> {N : M.Sig_alias} -> N.t) functor_alias_escape
       but a pattern was expected which matches values of type
         ({M : S} -> a) functor_alias_escape
       The module type M.Sig would escape its scope
|}]

let f (type a) (x : a) (y : a functor_alias_escape) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a functor_alias_escape -> 'a = <fun>
|}]

type _ functor_alias_expand =
  A : ({M : S} -> {N : M.S_outer_alias} -> N.t) functor_alias_expand;;

[%%expect{|
type _ functor_alias_expand =
    A : ({M : S} -> {N : M.S_outer_alias} -> N.t) functor_alias_expand
|}]

let functor_alias_expand (type a) (x : a)
    (y : ({M : S} -> a) functor_alias_expand) =
  match x, y with
  | x, A -> x = 15;;

[%%expect{|
Line 4, characters 16-18:
4 |   | x, A -> x = 15;;
                    ^^
Error: This expression has type int but an expression was expected of type
         a = {N : TYPE} -> N.t
|}]

let f (type a) (x : a) (y : a functor_alias_expand) =
  match x, y with
  | x, A -> x;;

[%%expect{|
val f : 'a -> 'a functor_alias_expand -> 'a = <fun>
|}]
