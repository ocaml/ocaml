(* TEST
  expect;
*)

module type Typ = sig type t end

module type Add = sig type t val add : t -> t -> t end

let id (module T : Typ) (x : T.t) = x

let id2 : (module T : Typ) -> T.t -> T.t =
  fun (module A : Typ) (x : A.t) -> x

let id_infer_sig : (module T : Typ) -> T.t -> T.t =
  fun (module A) (x : A.t) -> x

[%%expect{|
module type Typ = sig type t end
module type Add = sig type t val add : t -> t -> t end
val id : (module T : Typ) -> T.t -> T.t = <fun>
val id2 : (module T : Typ) -> T.t -> T.t = <fun>
val id_infer_sig : (module T : Typ) -> T.t -> T.t = <fun>
|}]


let f x y = (id (module Int) x, id (module Bool) y)

[%%expect{|
val f : Int.t -> Bool.t -> Int.t * Bool.t = <fun>
|}]

let merge (module T : Typ) x y = (id (module T) x, id (module T) y)

[%%expect{|
val merge : (module T : Typ) -> T.t -> T.t -> T.t * T.t = <fun>
|}]

let test_lambda a = (fun (module T : Typ) (x : T.t) -> x) (module Int) a

[%%expect{|
val test_lambda : Int.t -> Int.t = <fun>
|}]


let alpha_equiv (f : (module A : Add) -> A.t -> A.t)
  : (module T : Add) -> T.t -> T.t = f

[%%expect{|
val alpha_equiv :
  ((module A : Add) -> A.t -> A.t) -> (module T : Add) -> T.t -> T.t = <fun>
|}]

let apply_weird (module M : Typ) (f : (module M : Typ) -> _) (x : M.t) : M.t =
  f (module M) x

[%%expect{|
val apply_weird :
  (module M : Typ) -> ((module M : Typ) -> M/2.t -> M/2.t) -> M.t -> M.t =
  <fun>
|}]

(* Invalid arguments *)

let f x (module M : Typ) (y : M.t) = (x, y)

[%%expect{|
val f : 'a -> (module M : Typ) -> M.t -> 'a * M.t = <fun>
|}]

let invalid_arg1 = f (module Int)

[%%expect{|
Line 1, characters 21-33:
1 | let invalid_arg1 = f (module Int)
                         ^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let invalid_arg2 = f 3 4 (module Int)

[%%expect{|
Line 1, characters 23-24:
1 | let invalid_arg2 = f 3 4 (module Int)
                           ^
Error: This expression has type "int" but an expression was expected of type
         "(module Typ)"
|}]

let invalid_arg3 =
  let m = (module Int : Typ) in
  f 3 m 4

[%%expect{|
Line 3, characters 2-5:
3 |   f 3 m 4
      ^^^
Error: This expression has type "(module M : Typ) -> M.t -> 'a * M.t"
       but an expression was expected of type "(module Typ) -> 'b"
       The module "M" would escape its scope
|}]


let invalid_arg4 =
  let m = (module Int : Typ with type t = int) in
  f 3 m 4

[%%expect{|
Line 3, characters 6-7:
3 |   f 3 m 4
          ^
Error: This expression has type "(module Typ with type t = int)"
       but an expression was expected of type "(module Typ)"
|}]



let labelled (module M : Typ) ~(y:M.t) = y

let apply_labelled = labelled ~y:3 (module Int)

[%%expect{|
val labelled : (module M : Typ) -> y:M.t -> M.t = <fun>
val apply_labelled : Int.t = 3
|}]

let apply_labelled_fail = labelled ~y:3

[%%expect{|
Line 1, characters 26-34:
1 | let apply_labelled_fail = labelled ~y:3
                              ^^^^^^^^
Error: This expression has type "(module M : Typ) -> y:M.t -> M.t"
       Received an expression argument. However, module arguments cannot be omitted.
|}]

let apply_opt (f : ?opt:int -> (module M : Typ) -> M.t) = f (module Int)

[%%expect{|
val apply_opt : (?opt:int -> (module M : Typ) -> M.t) -> Int.t = <fun>
|}]

let build_pair (module M : Typ) ~x ~y : M.t * M.t = (x, y)

(* This raises a principality warning but maybe it shouldn't *)
let principality_warning = build_pair (module Int) ~y:3 ~x:1

[%%expect{|
val build_pair : (module M : Typ) -> x:M.t -> y:M.t -> M.t * M.t = <fun>
val principality_warning : Int.t * Int.t = (1, 3)
|}, Principal{|
val build_pair : (module M : Typ) -> x:M.t -> y:M.t -> M.t * M.t = <fun>
Line 4, characters 59-60:
4 | let principality_warning = build_pair (module Int) ~y:3 ~x:1
                                                               ^
Warning 18 [not-principal]: commuting this argument is not principal.

val principality_warning : Int.t * Int.t = (1, 3)
|}]

(* Typing rules make sense only if module argument are
   a path (module names, projections and applications) *)
let x_from_struct = id (module struct type t = int end) 3

[%%expect{|
Line 1, characters 23-55:
1 | let x_from_struct = id (module struct type t = int end) 3
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot infer path of module for functor.
|}]


module type Map = sig
  type _ t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

let map (module M : Map) f x = M.map f x

[%%expect{|
module type Map = sig type _ t val map : ('a -> 'b) -> 'a t -> 'b t end
val map : (module M : Map) -> ('a -> 'b) -> 'a M.t -> 'b M.t = <fun>
|}]


let s_list = map (module List) string_of_int [3; 1; 4]

[%%expect{|
val s_list : string List.t = ["3"; "1"; "4"]
|}]

let s_list : string list = s_list

[%%expect{|
val s_list : string list = ["3"; "1"; "4"]
|}]

module MapCombine (M1 : Map) (M2 : Map) = struct
  type 'a t = 'a M1.t M2.t
  let map f = map (module M2) (map (module M1) f)
end

let s_list_array = map (module MapCombine(List)(Array))
                       string_of_int [|[3; 2]; [2]; []|]

[%%expect{|
module MapCombine :
  functor (M1 : Map) (M2 : Map) ->
    sig
      type 'a t = 'a M1.t M2.t
      val map : ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t
    end
val s_list_array : string MapCombine(List)(Array).t =
  [|["3"; "2"]; ["2"]; []|]
|}]


let s_list_arrayb =
    map
      (module MapCombine(struct type 'a t = 'a list let map = List.map end)(Array))
      [|[3; 2]; [2]; []|]

[%%expect{|
Line 3, characters 6-83:
3 |       (module MapCombine(struct type 'a t = 'a list let map = List.map end)(Array))
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot infer path of module for functor.
|}]



(** Various tests on the coercion between functor types. **)
(* Here the sames rules as with first-class modules applies :
   coercion is allowed only if the runtime representation is the same.
*)

module type AddSub = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module type SubAdd = sig
  type t
  val sub : t -> t -> t
  val add : t -> t -> t
end

[%%expect{|
module type AddSub =
  sig type t val add : t -> t -> t val sub : t -> t -> t end
module type SubAdd =
  sig type t val sub : t -> t -> t val add : t -> t -> t end
|}]

module type Typ' = sig
  type t
end

let id3 : (module T : Typ') -> T.t -> T.t = id

[%%expect{|
module type Typ' = sig type t end
val id3 : (module T : Typ') -> T.t -> T.t = <fun>
|}]


let id4 = (id :> (module T : Typ) -> T.t -> T.t)

[%%expect{|
val id4 : (module T : Typ) -> T.t -> T.t = <fun>
|}]

let id5 = (id :> (module T : Typ') -> T.t -> T.t)

[%%expect{|
val id5 : (module T : Typ') -> T.t -> T.t = <fun>
|}]


(* Fails because this would require computation at runtime *)
let try_coerce (f : (module T : Typ) -> T.t -> T.t) =
  (f : (module A : Add) -> A.t -> A.t)

[%%expect{|
Line 2, characters 3-4:
2 |   (f : (module A : Add) -> A.t -> A.t)
       ^
Error: This expression has type "(module T : Typ) -> T.t -> T.t"
       but an expression was expected of type "(module A : Add) -> A.t -> A.t"
       Modules do not match: Typ is not included in Add
       The value "add" is required but not provided
|}]

let try_coerce' (f : (module T : Typ) -> T.t -> T.t) =
  (f :> (module A : Add) -> A.t -> A.t)

[%%expect{|
Line 2, characters 2-39:
2 |   (f :> (module A : Add) -> A.t -> A.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module T : Typ) -> T.t -> T.t" is not a subtype of
         "(module A : Add) -> A.t -> A.t"
       The two first-class module types differ by their runtime size.
|}]


(* Here the coercion requires computation and should? be forbidden *)
let try_coerce2 (f : (module A : AddSub) -> A.t -> A.t) =
    (f :> ((module T : SubAdd) -> T.t -> T.t))

[%%expect{|
Line 2, characters 4-46:
2 |     (f :> ((module T : SubAdd) -> T.t -> T.t))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module A : AddSub) -> A.t -> A.t" is not a subtype of
         "(module T : SubAdd) -> T.t -> T.t"
       The two first-class module types do not share
       the same positions for runtime components.
       For example, the value "sub" occurs at the expected position of
       the value "add".
|}]

module type Add2 = sig
  type a
  type t
  val add : t -> t -> t
end

module type Add3 = sig
  type t
  type a
  val add : t -> t -> t
end

module type Add4 = sig
  type t
  val add : t -> t -> t
  type a
end

[%%expect{|
module type Add2 = sig type a type t val add : t -> t -> t end
module type Add3 = sig type t type a val add : t -> t -> t end
module type Add4 = sig type t val add : t -> t -> t type a end
|}]

let try_coerce4 (f : (module A : Add) -> A.t -> A.t) =
    (f : (module A : Add2) -> A.t -> A.t)

[%%expect{|
Line 2, characters 5-6:
2 |     (f : (module A : Add2) -> A.t -> A.t)
         ^
Error: This expression has type "(module A : Add) -> A.t -> A.t"
       but an expression was expected of type "(module A : Add2) -> A.t -> A.t"
       Modules do not match: Add is not included in Add2
       The type "a" is required but not provided
|}]

let coerce5 (f : (module A : Add) -> A.t -> A.t) =
    (f :> (module A : Add2) -> A.t -> A.t)


(* changing type order in signature *)
let try_coerce6 (f : (module A : Add2) -> A.t -> A.t) =
    (f : (module A : Add3) -> A.t -> A.t)

let try_coerce7 (f : (module A : Add2) -> A.t -> A.t) =
  (f : (module A : Add4) -> A.t -> A.t)

[%%expect{|
val coerce5 :
  ((module A : Add) -> A.t -> A.t) -> (module A : Add2) -> A.t -> A.t = <fun>
val try_coerce6 :
  ((module A : Add2) -> A.t -> A.t) -> (module A : Add3) -> A.t -> A.t =
  <fun>
val try_coerce7 :
  ((module A : Add2) -> A.t -> A.t) -> (module A : Add4) -> A.t -> A.t =
  <fun>
|}]

(** Tests about unannoted applications *)

let apply f (module T : Typ) (x : T.t) : T.t = f (module T) x

[%%expect{|
Line 3, characters 49-59:
3 | let apply f (module T : Typ) (x : T.t) : T.t = f (module T) x
                                                     ^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let apply_with_annot f (module T : Typ) (x : T.t) : T.t =
  let _g : (module T : Typ) -> T.t -> T.t = f in
  f (module T) x

(* (type a) -> a -> a -> a *)
let merge_no_mod (type a) (x : a) (y : a) = x

[%%expect{|
val apply_with_annot :
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
val merge_no_mod : 'a -> 'a -> 'a = <fun>
|}, Principal{|
Line 3, characters 2-3:
3 |   f (module T) x
      ^
Warning 18 [not-principal]: applying a dependent function is not principal.

val apply_with_annot :
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
val merge_no_mod : 'a -> 'a -> 'a = <fun>
|}]


let apply_small_annot1 (f : (module T : Typ) -> T.t -> T.t) g (module T : Typ) x =
  let r = g (module T) x in
  let _ = merge_no_mod f g in
  r

[%%expect{|
Line 2, characters 12-22:
2 |   let r = g (module T) x in
                ^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]


let apply_small_annot2 (f : (module T : Typ) -> T.t -> T.t) g (module T : Typ) x =
  let _ = merge_no_mod f g in
  g (module T) x

[%%expect{|
val apply_small_annot2 :
  ((module T : Typ) -> T.t -> T.t) ->
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
|}, Principal{|
Line 3, characters 2-3:
3 |   g (module T) x
      ^
Warning 18 [not-principal]: applying a dependent function is not principal.

val apply_small_annot2 :
  ((module T : Typ) -> T.t -> T.t) ->
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
|}]


(* This is a syntax error *)
(* let id_bool_fail (module B : module type of Bool) (x : B.t) = x *)

module MyBool = struct
  type t = bool = false | true
  let not = Bool.not
end

module type TBool = module type of MyBool

let id_bool (module B : TBool) (x : B.t) = x

[%%expect{|
module MyBool : sig type t = bool = false | true val not : bool -> bool end
module type TBool =
  sig type t = bool = false | true val not : bool -> bool end
val id_bool : (module B : TBool) -> B.t -> B.t = <fun>
|}]


(** Escape errors **)

let r = ref None

let set (module T : Typ) (x : T.t) =
  r := Some x

[%%expect{|
val r : '_weak1 option ref = {contents = None}
Line 6, characters 12-13:
6 |   r := Some x
                ^
Error: This expression has type "T.t" but an expression was expected of type
         "'weak1"
       The type constructor "T.t" would escape its scope
|}]


let f x (module A : Add) (y : A.t) = A.add x y

[%%expect{|
Line 1, characters 43-44:
1 | let f x (module A : Add) (y : A.t) = A.add x y
                                               ^
Error: This expression has type "'a" but an expression was expected of type "A.t"
       The type constructor "A.t" would escape its scope
|}]

let f (x : (module T : Typ) -> _) : (module T : Typ) -> T.t = x

[%%expect{|
Line 1, characters 62-63:
1 | let f (x : (module T : Typ) -> _) : (module T : Typ) -> T.t = x
                                                                  ^
Error: This expression has type "(module T : Typ) -> 'a"
       but an expression was expected of type "(module T : Typ) -> T.t"
       The module "T" would escape its scope
|}]


(* Testing the `S with type t = _` cases *)

module type Coerce = sig
  type a
  type b
  val coerce : a -> b
end

let coerce (module C : Coerce) x = C.coerce x

module IntofBool = struct
  type a = bool
  type b = int
  let coerce b = if b then 1 else 0
end

module BoolofInt = struct
  type a = int
  type b = bool
  let coerce i = i <> 0
end

module IntofString = struct
  type a = string
  type b = int
  let coerce = int_of_string
end

module IntofFloat = struct
  type a = float
  type b = int
  let coerce = int_of_string_opt
end

[%%expect{|
module type Coerce = sig type a type b val coerce : a -> b end
val coerce : (module C : Coerce) -> C.a -> C.b = <fun>
module IntofBool :
  sig type a = bool type b = int val coerce : bool -> int end
module BoolofInt :
  sig type a = int type b = bool val coerce : int -> bool end
module IntofString :
  sig type a = string type b = int val coerce : string -> int end
module IntofFloat :
  sig type a = float type b = int val coerce : string -> int option end
|}]

let incr_general
  (module Cfrom : Coerce with type b = int)
  (module Cto : Coerce with type a = int and type b = Cfrom.a)
  x =
  coerce (module Cto) (1 + coerce (module Cfrom) x)

[%%expect{|
val incr_general :
  (module Cfrom : Coerce with type b = int) ->
  (module Cto : Coerce with type a = int and type b = Cfrom.a) ->
  Cfrom.a -> Cto.b = <fun>
|}]

module type CoerceToInt = sig
  type a
  type b = int
  val coerce : a -> int
end

module type CoerceFromInt = sig
  type a = int
  type b
  val coerce : int -> b
end

[%%expect{|
module type CoerceToInt = sig type a type b = int val coerce : a -> int end
module type CoerceFromInt = sig type a = int type b val coerce : int -> b end
|}]

let incr_general''
  = (incr_general :>
  (module C1 : CoerceToInt) -> (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a)

[%%expect{|
val incr_general'' :
  (module C1 : CoerceToInt) ->
  (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a = <fun>
|}]

let incr_general' :
  (module C1 : CoerceToInt) -> (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a =
  incr_general

[%%expect{|
val incr_general' :
  (module C1 : CoerceToInt) ->
  (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a = <fun>
|}]

(* Recursive and mutually recursive definitions *)

let rec f : (module T : Typ) -> int -> T.t -> T.t -> T.t =
  fun (module T : Typ) n (x : T.t) (y : T.t) ->
    if n = 0
    then x
    else f (module T) (n - 1) y x

[%%expect{|
val f : (module T : Typ) -> int -> T.t -> T.t -> T.t = <fun>
|}]

let rec f (module T : Typ) n (x : T.t) (y : T.t) =
  if n = 0
  then x
  else f (module T) (n - 1) y x

[%%expect{|
Line 4, characters 9-19:
4 |   else f (module T) (n - 1) y x
             ^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let rec f (module T : Typ) n (x : T.t) (y : T.t) =
  if n = 0
  then x
  else g (module T) x y
and g (module T : Typ) n (x : T.t) (y : T.t) =
  if n = 0
  then y
  else f (module T) x y

[%%expect{|
Line 4, characters 9-19:
4 |   else g (module T) x y
             ^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let rec m = map (module List) (fun x -> x) [3]

let rec m = map (module List) (fun x -> x) [3]
and g = 3 :: m

[%%expect{|
val m : int List.t = [3]
val m : int List.t = [3]
val g : int list = [3; 3]
|}]

let rec f (module T : Typ) x =
  g x
and g x = f (module Int) x

[%%expect{|
val f : (module Typ) -> 'a -> 'b = <fun>
val g : 'a -> 'b = <fun>
|}, Principal{|
Line 3, characters 12-24:
3 | and g x = f (module Int) x
                ^^^^^^^^^^^^
Warning 18 [not-principal]: this module packing is not principal.

val f : (module Typ) -> 'a -> 'b = <fun>
val g : 'a -> 'b = <fun>
|}]

let rec m = (fun (module T : Typ) (x : T.t) -> x) (module Int) 3

[%%expect{|
val m : Int.t = 3
|}]

(** Typing is impacted by typing order *)

let id' (f : (module T : Typ) -> T.t -> T.t) = f

let typing_order1 f = (f (module Int) 3, id' f)

[%%expect{|
val id' : ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t =
  <fun>
Line 5, characters 25-37:
5 | let typing_order1 f = (f (module Int) 3, id' f)
                             ^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

let typing_order2 f = (id' f, f (module Int) 3)

[%%expect{|
val typing_order2 :
  ((module T : Typ) -> T.t -> T.t) ->
  ((module T : Typ) -> T.t -> T.t) * Int.t = <fun>
|}, Principal{|
Line 1, characters 30-31:
1 | let typing_order2 f = (id' f, f (module Int) 3)
                                  ^
Warning 18 [not-principal]: applying a dependent function is not principal.

val typing_order2 :
  ((module T : Typ) -> T.t -> T.t) ->
  ((module T : Typ) -> T.t -> T.t) * Int.t = <fun>
|}]

(** The following test check that test at module unpacking still happen with
    modular explicits *)

(* we test that recursive types cannot occur *)
module type T = sig type t val v : t end;;
let foo (module X : T with type t = 'a) = X.v X.v;;

[%%expect{|
module type T = sig type t val v : t end
Line 6, characters 16-17:
6 | let foo (module X : T with type t = 'a) = X.v X.v;;
                    ^
Error: The type of this packed module contains variables:
       "(module T with type t = 'a)"
|}]

(* Test principality warning of type *)
let principality_warning2 f =
  let _ : ((module T : Typ) -> T.t -> T.t) -> unit = f in
  f (fun (module T) x -> x)

[%%expect{|
val principality_warning2 :
  (((module T : Typ) -> T.t -> T.t) -> unit) -> unit = <fun>
|}, Principal{|
Line 3, characters 9-27:
3 |   f (fun (module T) x -> x)
             ^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val principality_warning2 :
  (((module T : Typ) -> T.t -> T.t) -> unit) -> unit = <fun>
|}]
