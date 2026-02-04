(* TEST
  expect;
*)

module type Typ = sig type t end

module type Add = sig type t val add : t -> t -> t end

let id (module T : Typ) (x : T.t) = x

let id2 : (module T : Typ) -> T.t -> T.t =
  fun (module A : Typ) (x : A.t) -> x

let id_infer_sig  : (module T : Typ) -> T.t -> T.t =
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

let f2 x y = (id (module Int : Typ) x, id (module Bool : Typ) y)

[%%expect{|
val f2 : Int.t -> Bool.t -> Int.t * Bool.t = <fun>
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

(* Here we test that M is not captured inside the type of f *)
let apply_weird (module M : Typ) (f : (module M : Typ) -> _) (x : M.t) : M.t =
  f (module M) x

[%%expect{|
val apply_weird :
  (module M : Typ) -> ((module M : Typ) -> M/2.t -> M/2.t) -> M.t -> M.t =
  <fun>
|}]

(** From here on we will try applying invalid arguments to functions *)

let f x (module M : Typ) (y : M.t) = (x, y)

[%%expect{|
val f : 'a -> (module M : Typ) -> M.t -> 'a * M.t = <fun>
|}]

(* f does not constraint the type of the first argument of the function *)
let invalid_arg1 = f (module Int)

[%%expect{|
Line 1, characters 21-33:
1 | let invalid_arg1 = f (module Int)
                         ^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

(* We gave too many arguments before the module argument, resulting in a type
   error *)
let invalid_arg2 = f 3 4 (module Int)

[%%expect{|
Line 1, characters 23-24:
1 | let invalid_arg2 = f 3 4 (module Int)
                           ^
Error: The constant "4" has type "int" but an expression was expected of type
         "(module Typ)"
|}]

(* Here we cannot extract the type of m *)
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
This function is module-dependent. The dependency is preserved
when the function is passed a static module argument "(module M : S)"
or "(module M)". Its argument here is not static, so the type-checker
tried instead to change the function type to be non-dependent.
|}]

(* Here we cannot extract the type of m. This could be accepted because m does
   not hide any abstract types. *)
let invalid_arg4 =
  let m = (module Int : Typ with type t = int) in
  f 3 m 4

[%%expect{|
Line 3, characters 6-7:
3 |   f 3 m 4
          ^
Error: The value "m" has type "(module Typ with type t = int)"
       but an expression was expected of type "(module Typ)"
|}]

(** From here we will test things with labels *)

let labelled () (module M : Typ) ~(y:M.t) = y

let apply_labelled = labelled () ~y:3 (module Int)

[%%expect{|
val labelled : unit -> (module M : Typ) -> y:M.t -> M.t = <fun>
val apply_labelled : Int.t = 3
|}]

(* We cannot omit the module argument like other labelled arguments because of
   possible type dependancy *)
let apply_labelled_fail = labelled () ~y:3

[%%expect{|
Line 1, characters 26-37:
1 | let apply_labelled_fail = labelled () ~y:3
                              ^^^^^^^^^^^
Error: This function has type "(module M : Typ) -> y:M.t -> M.t"
       The module argument "M" cannot be omitted in this application.
|}]

(* Here we can remove the dependancy, thus it should be accepted. *)
let labelled' (module M : Typ with type t = int) ~(y:M.t) = y

let apply_labelled_success = labelled' ~y:3

[%%expect{|
val labelled' : (module M : Typ with type t = int) -> y:M.t -> M.t = <fun>
val apply_labelled_success : (module Typ with type t = int) -> int = <fun>
|}]

(* Check that the optionnal argument is removed correctly when applying a
module argument. *)
let apply_opt (f : ?opt:int -> (module M : Typ) -> M.t) = f (module Int)

[%%expect{|
val apply_opt : (?opt:int -> (module M : Typ) -> M.t) -> Int.t = <fun>
|}]

let build_pair (module M : Typ) ~x ~y : M.t * M.t = (x, y)

(* This shouldn't raise a principality warning *)
let test_principality_of_commuting_labels = build_pair (module Int) ~y:3 ~x:1

[%%expect{|
val build_pair : (module M : Typ) -> x:M.t -> y:M.t -> M.t * M.t = <fun>
val test_principality_of_commuting_labels : Int.t * Int.t = (1, 3)
|}]

let foo f a =
  let _ = (f ~a : (module M : Typ) -> M.t) in
  f ~a (fun x -> x)

[%%expect{|
Line 3, characters 7-19:
3 |   f ~a (fun x -> x)
           ^^^^^^^^^^^^
Error: This expression should not be a function, the expected type is
       "(module Typ)"
|}]

let foo2 f a =
  let m = (module Int : Typ) in
  let _ = (f ~a : (module M : Typ) -> M.t) in
  f ~a m

[%%expect{|
Line 4, characters 2-6:
4 |   f ~a m
      ^^^^
Error: This expression has type "(module M : Typ) -> M.t"
       but an expression was expected of type "(module Typ) -> 'a"
       The module "M" would escape its scope
This function is module-dependent. The dependency is preserved
when the function is passed a static module argument "(module M : S)"
or "(module M)". Its argument here is not static, so the type-checker
tried instead to change the function type to be non-dependent.
|}]

let foo3 f a =
  let _ = (f ~a : (module M : Typ) -> M.t) in
  f ~a (module Int)

[%%expect{|
val foo3 : (a:'a -> (module M : Typ) -> M.t) -> 'a -> Int.t = <fun>
|}]

let foo4 f a =
  let _ = (f ~a : b:(module M : Typ) -> M.t) in
  f ~a (module Int)

[%%expect{|
Line 3, characters 7-19:
3 |   f ~a (module Int)
           ^^^^^^^^^^^^
Error: The function applied to this argument has type
         b:(module M : Typ) -> M.t
This argument cannot be applied without label
|}]


let foo4 f a =
  let _ = (f ~a : b:(module M : Typ) -> M.t) in
  f ~a ~c:(module Int)

[%%expect{|
Line 3, characters 10-22:
3 |   f ~a ~c:(module Int)
              ^^^^^^^^^^^^
Error: The function applied to this argument has type
         b:(module M : Typ) -> M.t
This argument cannot be applied with label "~c"
|}]

(** From here we test possible expressions for the module argument. *)

let x_from_struct = id (module struct type t = int end) 3

[%%expect{|
val x_from_struct : int = 3
|}]

module F () : Typ = struct type t = int end

let x_from_generative_functor = id (module F ())

[%%expect{|
module F : () -> Typ
Line 3, characters 32-34:
3 | let x_from_generative_functor = id (module F ())
                                    ^^
Error: This expression has type "(module T : Typ) -> T.t -> T.t"
       but an expression was expected of type "(module Typ) -> 'a"
       The module "T" would escape its scope
This function is module-dependent. The dependency is preserved
when the function is passed a static module argument "(module M : S)"
or "(module M)". Its argument here is not static, so the type-checker
tried instead to change the function type to be non-dependent.
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

(** Testing functor application *)

module MapCombine (M1 : Map) (M2 : Map) = struct
  type 'a t = 'a M1.t M2.t
  let map f = map (module M2) (map (module M1) f)
end

let s_list_array = map (module MapCombine(List)(Array))
                       string_of_int [|[3; 2]; [2]; []|]

[%%expect{|
module MapCombine :
  (M1 : Map) (M2 : Map) ->
    sig
      type 'a t = 'a M1.t M2.t
      val map : ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t
    end
val s_list_array : string MapCombine(List)(Array).t =
  [|["3"; "2"]; ["2"]; []|]
|}]

(* Checks that a structure as functor argument is rejected if
   an abstract type is created. *)

let s_list_arrayb =
    map
      (module MapCombine(struct
          type 'a t = A of 'a
          let map f (A x) = (A (f x))
        end)(Array))
      string_of_int [|[]|]

[%%expect{|
Lines 3-6, characters 14-12:
3 | ..............MapCombine(struct
4 |           type 'a t = A of 'a
5 |           let map f (A x) = (A (f x))
6 |         end)........
Error: This functor has type
       "(M1 : Map) (M2 : Map) ->
         sig
           type 'a t = 'a M1.t M2.t
           val map : ('a -> 'b) -> 'a M1.t M2.t -> 'b M1.t M2.t
         end"
       The parameter cannot be eliminated in the result type.
       Please bind the argument to a module identifier.
|}]

(* Checks that a structure as functor argument can be accepted if
   no abstract types are created. *)

let s_list_arrayb =
    map
      (module MapCombine(struct type 'a t = 'a list let map = List.map end)(Array))
      string_of_int [|[3; 2]; [2]; []|]

[%%expect{|
val s_list_arrayb : string list Array.t = [|["3"; "2"]; ["2"]; []|]
|}]

module F () : Map = struct
  type 'a t = 'a list
  let map = List.map
end

let fail = map (module F()) string_of_int [3]

[%%expect{|
module F : () -> Map
Line 6, characters 11-14:
6 | let fail = map (module F()) string_of_int [3]
               ^^^
Error: This expression has type
         "(module M : Map) -> ('a -> 'b) -> 'a M.t -> 'b M.t"
       but an expression was expected of type "(module Map) -> 'c"
       The module "M" would escape its scope
This function is module-dependent. The dependency is preserved
when the function is passed a static module argument "(module M : S)"
or "(module M)". Its argument here is not static, so the type-checker
tried instead to change the function type to be non-dependent.
|}]

(* The example above is accepted if no abstract types are created *)
module F () = struct
  type 'a t = 'a list
  let map = List.map
end

let ok = map (module F()) string_of_int [3]

[%%expect{|
module F :
  () ->
    sig type 'a t = 'a list val map : ('a -> 'b) -> 'a list -> 'b list end
val ok : string list = ["3"]
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

(* Same signature but with a different name *)
let id3 : (module T : Typ') -> T.t -> T.t = id

(* Reflexivity of ground coercion *)
let id4 = (id :> (module T : Typ) -> T.t -> T.t)

(* Ground coercion for same signature but a different name *)
let id5 = (id :> (module T : Typ') -> T.t -> T.t)

[%%expect{|
module type Typ' = sig type t end
val id3 : (module T : Typ') -> T.t -> T.t = <fun>
val id4 : (module T : Typ) -> T.t -> T.t = <fun>
val id5 : (module T : Typ') -> T.t -> T.t = <fun>
|}]

(* Fails because this would require computation at runtime *)
let try_unify (f : (module T : Typ) -> T.t -> T.t) =
  (f : (module A : Add) -> A.t -> A.t)

[%%expect{|
Line 2, characters 3-4:
2 |   (f : (module A : Add) -> A.t -> A.t)
       ^
Error: The value "f" has type "(module T : Typ) -> T.t -> T.t"
       but an expression was expected of type "(module A : Add) -> A.t -> A.t"
       Modules do not match: Typ is not included in Add
       The value "add" is required but not provided
|}]

(* This also fails with ground coercion *)
let try_coerce (f : (module T : Typ) -> T.t -> T.t) =
  (f :> (module A : Add) -> A.t -> A.t)

[%%expect{|
Line 2, characters 2-39:
2 |   (f :> (module A : Add) -> A.t -> A.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module T : Typ) -> T.t -> T.t" is not a subtype of
         "(module A : Add) -> A.t -> A.t"
       The two first-class module types differ by their runtime size.
|}]

(* Fails because this would require computation at runtime *)
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

(* Unification does not allow changing the signature *)
let try_coerce4 (f : (module A : Add) -> A.t -> A.t) =
    (f : (module A : Add2) -> A.t -> A.t)

[%%expect{|
Line 2, characters 5-6:
2 |     (f : (module A : Add2) -> A.t -> A.t)
         ^
Error: The value "f" has type "(module A : Add) -> A.t -> A.t"
       but an expression was expected of type "(module A : Add2) -> A.t -> A.t"
       Modules do not match: Add is not included in Add2
       The type "a" is required but not provided
|}]

(* But we can add type fields with ground coercion *)
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

(* We cannot decrease the signature *)
let try_coerce8 (f : (module A : Add2) -> A.t -> A.t) =
  (f :> (module A : Add) -> A.t -> A.t)

[%%expect{|
Line 2, characters 2-39:
2 |   (f :> (module A : Add) -> A.t -> A.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module A : Add2) -> A.t -> A.t" is not a subtype of
         "(module A : Add) -> A.t -> A.t"
       Modules do not match: Add is not included in Add2
       The type "a" is required but not provided
|}]

(* Test coercions with additionnal infos *)

let restrict_signature1 (x : (module T : Typ) -> T.t -> T.t) =
  (x :> (module T : Typ with type t = int) -> T.t -> T.t)

let restrict_signature2 (x : (module T : Typ) -> T.t -> T.t) =
    (x :> (module T : Typ with type t = int) -> int -> int)

let restrict_signature_to_remove_dep (x : (module T : Typ) -> T.t -> T.t) =
    (x :> (module Typ with type t = int) -> int -> int)

let restrict_signature_to_add_dep (x : (module Typ) -> int -> int) =
  (x :> (module T : Typ with type t = int) -> T.t -> T.t)

[%%expect{|
val restrict_signature1 :
  ((module T : Typ) -> T.t -> T.t) ->
  (module T : Typ with type t = int) -> T.t -> T.t = <fun>
val restrict_signature2 :
  ((module T : Typ) -> T.t -> T.t) ->
  (module T : Typ with type t = int) -> int -> int = <fun>
val restrict_signature_to_remove_dep :
  ((module T : Typ) -> T.t -> T.t) ->
  (module Typ with type t = int) -> int -> int = <fun>
val restrict_signature_to_add_dep :
  ((module Typ) -> int -> int) ->
  (module T : Typ with type t = int) -> T.t -> T.t = <fun>
|}]

module type TypPrivInt = sig
  type t = private int
end

let restrict_signature_with_priv_to_remove_dep =
  fun (x : (module T : Typ) -> unit -> T.t) ->
    (x :> (module TypPrivInt) -> unit -> int)

let restrict_signature_with_priv_to_add_dep =
  fun (x : (module Typ) -> int -> int) ->
  (x :> (module T : TypPrivInt) -> T.t -> int)


[%%expect{|
module type TypPrivInt = sig type t = private int end
val restrict_signature_with_priv_to_remove_dep :
  ((module T : Typ) -> unit -> T.t) -> (module TypPrivInt) -> unit -> int =
  <fun>
val restrict_signature_with_priv_to_add_dep :
  ((module Typ) -> int -> int) -> (module T : TypPrivInt) -> T.t -> int =
  <fun>
|}]

module PrivateFCM = struct
  type t = private (module Typ with type t = int)
end

let subtyping_to_private_fcm x =
  (x : (module M : Typ) -> M.t :> PrivateFCM.t -> int)

[%%expect{|
module PrivateFCM : sig type t = private (module Typ with type t = int) end
val subtyping_to_private_fcm :
  ((module M : Typ) -> M.t) -> PrivateFCM.t -> int = <fun>
|}]

let failed_subtyping x =
  (x : (module A : Typ) -> A.t list :> (module B : Typ) -> B.t)

[%%expect{|
Line 2, characters 2-63:
2 |   (x : (module A : Typ) -> A.t list :> (module B : Typ) -> B.t)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "(module A : Typ) -> A.t list" is not a subtype of
         "(module B : Typ) -> B.t"
       Type "A.t list" is not a subtype of "B.t"
|}]

class type ct = object end

let test_build_subtype x =
  let _ : (module T : Typ) -> 'a -> T.t -> < m : int > = x in
  (x :> (module T : Typ) -> int -> T.t -> ct)

[%%expect{|
class type ct = object  end
val test_build_subtype :
  ((module T : Typ) -> int -> T.t -> < m : int >) ->
  (module T : Typ) -> int -> T.t -> ct = <fun>
|}]

(* Test moregen *)

module M : sig
  val f1 : unit -> (module M : Typ) -> 'a -> 'a
  val f : (module M : Typ) -> M.t -> M.t
end = struct
  let f1 () (module M : Typ) : 'a -> 'a = assert false
  (* unit -> (module M : T) -> 'a -> 'a *)
  let f = f1 ()
  (* (module M : T) -> '_weak -> '_weak *)
end

[%%expect{|
Lines 4-9, characters 6-3:
4 | ......struct
5 |   let f1 () (module M : Typ) : 'a -> 'a = assert false
6 |   (* unit -> (module M : T) -> 'a -> 'a *)
7 |   let f = f1 ()
8 |   (* (module M : T) -> '_weak -> '_weak *)
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val f1 : unit -> (module Typ) -> 'a -> 'a
           val f : (module Typ) -> '_weak1 -> '_weak1
         end
       is not included in
         sig
           val f1 : unit -> (module M : Typ) -> 'a -> 'a
           val f : (module M : Typ) -> M.t -> M.t
         end
       Values do not match:
         val f : (module Typ) -> '_weak1 -> '_weak1
       is not included in
         val f : (module M : Typ) -> M.t -> M.t
       The type "(module Typ) -> '_weak1 -> '_weak1"
       is not compatible with the type "(module M : Typ) -> M.t -> M.t"
       The module "M" would escape its scope
|}]

(* Test if type subtyping and unification also works with types being
  a first-class module hidden behind a path *)

type mod_with_int = (module Typ with type t = int)

type mod_without_cstrs = (module Typ)

let restrict_signature_in_path (x : (module T : Typ) -> T.t -> T.t) =
  (x :> mod_with_int -> int -> int)

let restrict_signature_in_path2 (x : mod_without_cstrs -> int -> int) =
  (x :> (module T : Typ with type t = int) -> T.t -> T.t)

let basic_subtyping (x : (module T : Typ with type t = int) -> T.t -> T.t) =
  (x : mod_with_int -> int -> int)

let basic_subtyping2 (x : mod_with_int -> int -> int) =
  (x : (module T : Typ with type t = int) -> T.t -> T.t)

[%%expect{|
type mod_with_int = (module Typ with type t = int)
type mod_without_cstrs = (module Typ)
val restrict_signature_in_path :
  ((module T : Typ) -> T.t -> T.t) -> mod_with_int -> int -> int = <fun>
val restrict_signature_in_path2 :
  (mod_without_cstrs -> int -> int) ->
  (module T : Typ with type t = int) -> T.t -> T.t = <fun>
val basic_subtyping :
  ((module T : Typ with type t = int) -> T.t -> T.t) ->
  mod_with_int -> int -> int = <fun>
val basic_subtyping2 :
  (mod_with_int -> int -> int) ->
  (module T : Typ with type t = int) -> T.t -> T.t = <fun>
|}]

(* Small test to ensure subtyping does not expect the arrow argument to
   be a module *)
let subtyping_fail (x : (module T : Typ) -> T.t) =
    (x :> int -> int)

[%%expect{|
Line 2, characters 4-21:
2 |     (x :> int -> int)
        ^^^^^^^^^^^^^^^^^
Error: Type "(module T : Typ) -> T.t" is not a subtype of "int -> int"
       Type "int" is not a subtype of "(module Typ)"
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

[%%expect{|
val apply_with_annot :
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
|}, Principal{|
Line 3, characters 2-3:
3 |   f (module T) x
      ^
Warning 18 [not-principal]: applying a dependent function is not principal.

val apply_with_annot :
  ((module T : Typ) -> T.t -> T.t) -> (module T : Typ) -> T.t -> T.t = <fun>
|}]

(* Used to propagate type annotations  *)
let merge_no_mod (type a) (x : a) (y : a) = x

[%%expect{|
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

let _ = id_bool (module MyBool) MyBool.(false)

[%%expect{|
module MyBool : sig type t = bool = false | true val not : bool -> bool end
module type TBool =
  sig type t = bool = false | true val not : bool -> bool end
val id_bool : (module B : TBool) -> B.t -> B.t = <fun>
- : MyBool.t = MyBool.(false)
|}]


(** Escape errors **)

let r = ref None

let set (module T : Typ) (x : T.t) =
  r := Some x

[%%expect{|
val r : '_weak2 option ref = {contents = None}
Line 6, characters 12-13:
6 |   r := Some x
                ^
Error: The value "x" has type "T.t" but an expression was expected of type "'weak2"
       The type constructor "T.t" would escape its scope
|}]


let f x (module A : Add) (y : A.t) = A.add x y

[%%expect{|
Line 1, characters 43-44:
1 | let f x (module A : Add) (y : A.t) = A.add x y
                                               ^
Error: The value "x" has type "'a" but an expression was expected of type "A.t"
       The type constructor "A.t" would escape its scope
|}]

let f (x : (module T : Typ) -> _) : (module T : Typ) -> T.t = x

[%%expect{|
Line 1, characters 62-63:
1 | let f (x : (module T : Typ) -> _) : (module T : Typ) -> T.t = x
                                                                  ^
Error: The value "x" has type "(module T : Typ) -> 'a"
       but an expression was expected of type "(module T : Typ) -> T.t"
       The module "T" would escape its scope
|}]


(** Testing the `S with type t = _` cases *)

module type Coerce = sig
  type a
  type b
  val coerce : a -> b
end

let coerce (module C : Coerce) x = C.coerce x

[%%expect{|
module type Coerce = sig type a type b val coerce : a -> b end
val coerce : (module C : Coerce) -> C.a -> C.b = <fun>
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

let incr_general'
  = (incr_general :
  (module C1 : CoerceToInt) -> (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a)

[%%expect{|
val incr_general' :
  (module C1 : CoerceToInt) ->
  (module CoerceFromInt with type b = C1.a) -> C1.a -> C1.a = <fun>
|}]

(* Test that variance is correctly used during subtyping *)

module type InvTy = sig type 'a t end
module type CovTy = sig type +'a t end
type 'a t = (module M : CovTy) -> ((module N : InvTy) -> 'a M.t N.t) -> unit
type 'a s = (module M : InvTy) -> ((module N : CovTy) -> 'a M.t N.t) -> unit
[%%expect{|
module type InvTy = sig type 'a t end
module type CovTy = sig type +'a t end
type 'a t = (module M : CovTy) -> ((module N : InvTy) -> 'a M.t N.t) -> unit
type 'a s = (module M : InvTy) -> ((module N : CovTy) -> 'a M.t N.t) -> unit
|}]

(* When subtyping s to t, 'a M.t N.t is covariant in 'a because the relevant M,
   N modules are both CovTy, so st has a more general type than ss or tt *)
let ss x = (x : [>`A] s :> [`A] s)
let tt x = (x : [>`A] t :> [`A] t)
let st x = (x : [>`A] s :> [`A] t)
[%%expect{|
val ss : [ `A ] s -> [ `A ] s = <fun>
val tt : [ `A ] t -> [ `A ] t = <fun>
val st : [> `A ] s -> [ `A ] t = <fun>
|}]

(* Same as st above, but via eta-expansion instead of subtyping *)
let st' (s : [>`A] s) : [`A] t =
  fun (module M) f ->
  s (module M)
    (fun (module N) -> (f (module N) : [`A] M.t N.t :> [> `A] M.t N.t))

[%%expect{|
val st' : [> `A ] s -> [ `A ] t = <fun>
|}]

(** Recursive and mutually recursive definitions *)

let rec f : (module T : Typ) -> int -> T.t -> T.t -> T.t =
  fun (module T) n (x : T.t) (y : T.t) ->
    if n = 0
    then x
    else f (module T) (n - 1) y x

[%%expect{|
val f : (module T : Typ) -> int -> T.t -> T.t -> T.t = <fun>
|}]

(* Type cannot be infered because type approximation for letrecs is partial. *)
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

(* Type cannot be infered because type approximation for letrecs is partial. *)
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

(* This test is similar to the previous one without the error in f definition *)
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

(* Test that the value letrecs does not trivially fails on dependant
  applications *)
let rec m = map (module List) (fun x -> x) [3]

let rec m = map (module List) (fun x -> x) [3]
and g = 3 :: m

let rec m = (fun (module T : Typ) (x : T.t) -> x) (module Int) 3

[%%expect{|
val m : int List.t = [3]
val m : int List.t = [3]
val g : int list = [3; 3]
val m : Int.t = 3
|}]

(** Typing is impacted by typing order, the following tests show this. *)

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

(** The following test check that tests at module unpacking still happen with
    modular explicits *)

(* we test that free type variables cannot occur *)
module type T = sig type t val v : t end;;
let foo (module X : T with type t = 'a) = X.v X.v;;

[%%expect{|
module type T = sig type t val v : t end
Line 6, characters 8-39:
6 | let foo (module X : T with type t = 'a) = X.v X.v;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
Line 3, characters 9-19:
3 |   f (fun (module T) x -> x)
             ^^^^^^^^^^
Warning 18 [not-principal]: this module unpacking is not principal.

val principality_warning2 :
  (((module T : Typ) -> T.t -> T.t) -> unit) -> unit = <fun>
|}]

(*
    Ensure that application of a module-dependent function does not erase the
    principality of the return type.
*)
let should_be_principal (f : (module M : Typ) -> (module N : Typ) -> M.t * N.t)
  = f (module Int) (module Int)

[%%expect{|
val should_be_principal :
  ((module M : Typ) -> (module N : Typ) -> M.t * N.t) -> Int.t * Int.t =
  <fun>
|}]

(* This test check that as `t` is private we cannot inline its definition *)
module type S = sig
  type t = private int
  val f : t
end

let check_escape : _ -> _ = fun (module M : S) -> M.f

[%%expect{|
module type S = sig type t = private int val f : t end
Line 6, characters 50-53:
6 | let check_escape : _ -> _ = fun (module M : S) -> M.f
                                                      ^^^
Error: The value "M.f" has type "M.t" but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}]

(* The following test should give a warning only once.
  Here we test that the structure is typed only once.
  To achieve this we wrote a code that raises a warning when typed.
  If the warning is raised twice then that means typing happened twice.
*)

module type TInt = sig type t = int end

let f (module T : TInt) (x : T.t) = x

let raise_principality_warning =
  f (module struct
      type t = int
      let dummy_value = let x = 3 in 0
    end)

[%%expect{|
module type TInt = sig type t = int end
val f : (module T : TInt) -> T.t -> T.t = <fun>
Line 8, characters 28-29:
8 |       let dummy_value = let x = 3 in 0
                                ^
Warning 26 [unused-var]: unused variable "x".

val raise_principality_warning : int -> int = <fun>
|}]

let test_instance_nondep f =
  let _ : (module M : Typ) -> M.t = f in
  ignore (f (module struct type t = int end));
  f

[%%expect{|
val test_instance_nondep :
  ((module M : Typ) -> M.t) -> (module M : Typ) -> M.t = <fun>
|}, Principal{|
Line 3, characters 10-11:
3 |   ignore (f (module struct type t = int end));
              ^
Warning 18 [not-principal]: applying a dependent function is not principal.

val test_instance_nondep :
  ((module M : Typ) -> M.t) -> (module M : Typ) -> M.t = <fun>
|}]

(* Test relaxed value restriction *)

module type Covariant = sig
  type +'a t
end

module type Contravariant = sig
  type -'a t
end

let f_covar () (module M : Covariant) : 'a M.t = assert false
let f_contra () (module M : Contravariant) : 'a M.t = assert false

[%%expect{|
module type Covariant = sig type +'a t end
module type Contravariant = sig type -'a t end
val f_covar : unit -> (module M : Covariant) -> 'a M.t = <fun>
val f_contra : unit -> (module M : Contravariant) -> 'a M.t = <fun>
|}]

let f_covar_applied = f_covar ()
let f_contra_applied = f_contra ()

[%%expect{|
val f_covar_applied : (module M : Covariant) -> 'a M.t = <fun>
val f_contra_applied : (module M : Contravariant) -> '_weak3 M.t = <fun>
|}]

module type M_arrow1 = sig
  type 'a t = int -> 'a
end

let fa1 () (module M : M_arrow1) : 'a M.t = assert false

let fa1_applied = fa1 ()

[%%expect{|
module type M_arrow1 = sig type 'a t = int -> 'a end
val fa1 : unit -> (module M : M_arrow1) -> 'a M.t = <fun>
val fa1_applied : (module M : M_arrow1) -> 'a M.t = <fun>
|}]

module type M_arrow2 = sig
  type 'a t = 'a -> int
end

let fa2 () (module M : M_arrow2) : 'a M.t = assert false

let fa2_applied = fa2 ()

[%%expect{|
module type M_arrow2 = sig type 'a t = 'a -> int end
val fa2 : unit -> (module M : M_arrow2) -> 'a M.t = <fun>
val fa2_applied : (module M : M_arrow2) -> '_weak4 M.t = <fun>
|}]

module type Typ2 = sig
  type +'a tp
  type -'a tm
  type +!'a tpb
  type -!'a tmb
  type !'a tb
  type 'a t
end

let ftp  () (module M : Typ2) : 'a M.tp  = assert false
let ftm  () (module M : Typ2) : 'a M.tm  = assert false
let ftpb () (module M : Typ2) : 'a M.tpb = assert false
let ftmb () (module M : Typ2) : 'a M.tmb = assert false
let ftb  () (module M : Typ2) : 'a M.tb  = assert false
let ft   () (module M : Typ2) : 'a M.t   = assert false

let ftp_applied  = ftp ()
let ftm_applied  = ftm ()
let ftpb_applied = ftpb ()
let ftmb_applied = ftmb ()
let ftb_applied  = ftb ()
let ft_applied   = ft ()

[%%expect{|
module type Typ2 =
  sig
    type +'a tp
    type -'a tm
    type +!'a tpb
    type -!'a tmb
    type !'a tb
    type 'a t
  end
val ftp : unit -> (module M : Typ2) -> 'a M.tp = <fun>
val ftm : unit -> (module M : Typ2) -> 'a M.tm = <fun>
val ftpb : unit -> (module M : Typ2) -> 'a M.tpb = <fun>
val ftmb : unit -> (module M : Typ2) -> 'a M.tmb = <fun>
val ftb : unit -> (module M : Typ2) -> 'a M.tb = <fun>
val ft : unit -> (module M : Typ2) -> 'a M.t = <fun>
val ftp_applied : (module M : Typ2) -> 'a M.tp = <fun>
val ftm_applied : (module M : Typ2) -> '_weak5 M.tm = <fun>
val ftpb_applied : (module M : Typ2) -> 'a M.tpb = <fun>
val ftmb_applied : (module M : Typ2) -> '_weak6 M.tmb = <fun>
val ftb_applied : (module M : Typ2) -> '_weak7 M.tb = <fun>
val ft_applied : (module M : Typ2) -> '_weak8 M.t = <fun>
|}]



let f3 (type a) () (module T : Typ with type t = a) = ()

let f3_applied = f3 ()

[%%expect{|
val f3 : unit -> (module Typ with type t = 'a) -> unit = <fun>
val f3_applied : (module Typ with type t = '_weak9) -> unit = <fun>
|}]

(* Ensure that subst handles module dependent functions *)
module type S = sig end
module type Ty = sig type 'a t ;; val x : int t end
module type Boom = Ty with type 'a t := (module M : S) -> 'a

[%%expect{|
module type S = sig end
module type Ty = sig type 'a t val x : int t end
module type Boom = sig val x : (module M : S) -> int end
|}]

(** Tests with external functions *)

external external1 : (module M : Typ) -> (module Typ) = "%identity"

[%%expect{|
Line 3, characters 21-53:
3 | external external1 : (module M : Typ) -> (module Typ) = "%identity"
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type "(module M : Typ) -> (module Typ)"
       cannot be used to annotate an external function.
|}]

external external2 : ((module M : Typ) -> (module Typ) as 'a) -> 'a
  = "%identity"

[%%expect{|
Line 1, characters 65-67:
1 | external external2 : ((module M : Typ) -> (module Typ) as 'a) -> 'a
                                                                     ^^
Error: This external declaration has a non-syntactic arity,
       its arity is greater than its syntactic arity.
|}]

(** Test printing of long trace. *)

(* The goal here is to shadow Int with unification in order to create an
   ambigous error message.
*)
let f (x : (module T : Typ) -> int -> Int.t)
  : (module Int : Typ) -> int -> Int.t
  = x

(* This test does not work as intended. *)
[%%expect{|
Line 8, characters 4-5:
8 |   = x
        ^
Error: The value "x" has type "(module Int : Typ) -> int -> Stdlib.Int.t"
       but an expression was expected of type
         "(module Int : Typ) -> int -> Int.t"
       Type "Int.t" = "int" is not compatible with type "Int.t"
|}]

(* At one point the implementation was not robust enough and linking
  interacted badly with linking identifiers together. *)

let linking_ident1 (x : (module A : Typ with type t = int) -> int) =
  (x : (module Z : Typ with type t = int) -> Z.t)

let linking_ident2 (x : (module Z : Typ with type t = int) -> Z.t) =
  (x : (module A : Typ with type t = int) -> int)

[%%expect{|
val linking_ident1 :
  ((module A : Typ with type t = int) -> int) ->
  (module Z : Typ with type t = int) -> Z.t = <fun>
val linking_ident2 :
  ((module Z : Typ with type t = int) -> Z.t) ->
  (module A : Typ with type t = int) -> int = <fun>
|}]

let test_filter_arrow
  : (module M : Typ with type t = int) -> M.t
  = fun m -> 3

[%%expect{|
val test_filter_arrow : (module M : Typ with type t = int) -> M.t = <fun>
|}]

let test_failing_filter_arrow
  : (module M : Typ) -> M.t
  = fun m -> assert false

[%%expect{|
Line 3, characters 4-25:
3 |   = fun m -> assert false
        ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(module Typ) -> 'a"
       but an expression was expected of type "(module M : Typ) -> M.t"
       The module "M" would escape its scope
|}]

(* Here we test that the short-path mechanism does not
   replace [Avoid__me.t] by [M.t]. *)
module Avoid__me = struct type t end
module type S = sig type t = Avoid__me.t end
let f: (module M:S) -> Avoid__me.t -> unit = fun _ _ -> ()

[%%expect{|
module Avoid__me : sig type t end
module type S = sig type t = Avoid__me.t end
val f : (module M : S) -> Avoid__me.t -> unit = <fun>
|}]

(* We would expect the second example to be accepted but this behaviour
   is the same with non-dependent functions.
*)

let ok (x: [ `Y | `X of ((module M : Typ) -> M.t -> M.t) ]) = match x with
  | `X f -> f (module Int) 0
  | `Y -> 0

let fail (x: [< `Y | `X of ((module M : Typ) -> M.t -> M.t) ]) = match x with
  | `X f -> f (module Int) 0
  | `Y -> 0

[%%expect{|
val ok : [ `X of (module M : Typ) -> M.t -> M.t | `Y ] -> Int.t = <fun>
Line 6, characters 14-26:
6 |   | `X f -> f (module Int) 0
                  ^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}]

module type T = sig
  type a = int
 type v = [ `A of a ]
end
module M = struct type a = int type v = [`A of a ] end
let f: (module M:T) -> ([> M.v ] as 'a) -> 'a = fun _ x -> x
let u f  = (f: (module M : T) -> ([> M.v ] as 'a) -> 'a :> (module T) -> _ -> _)

[%%expect{|
module type T = sig type a = int type v = [ `A of a ] end
module M : sig type a = int type v = [ `A of a ] end
val f : (module M : T) -> ([> M.v ] as 'a) -> 'a = <fun>
val u : ((module M : T) -> ([> M.v ] as 'a) -> 'a) -> (module T) -> 'a -> 'a =
  <fun>
|}]
