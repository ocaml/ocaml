(* TEST
   * expect
*)

module M : sig type +!'a t end = struct type 'a t = 'a list end
[%%expect{|
module M : sig type +!'a t end
|}]
type _ t = M : 'a -> 'a M.t t (* OK *)
[%%expect{|
type _ t = M : 'a -> 'a M.t t
|}]

module N : sig type +'a t end = struct type 'a t = 'a list end
[%%expect{|
module N : sig type +'a t end
|}]
type _ t = N : 'a -> 'a N.t t (* KO *)
[%%expect{|
Line 1, characters 0-29:
1 | type _ t = N : 'a -> 'a N.t t (* KO *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]

module M : sig type +!'a t end = struct type 'a t = int end (* KO *)
[%%expect{|
Line 1, characters 33-59:
1 | module M : sig type +!'a t end = struct type 'a t = int end (* KO *)
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = int end
       is not included in
         sig type +!'a t end
       Type declarations do not match:
         type 'a t = int
       is not included in
         type +!'a t
       Their variances do not agree.
|}]

(* Motivating examples with GADTs *)

type (_,_) eq = Refl : ('a,'a) eq

module Vec : sig
  type +!'a t
  val make : int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
end = struct
  type 'a t = Vec of Obj.t array
  let make n f = Vec (Obj.magic Array.init n f)
  let get (Vec v) n = Obj.obj (Array.get v n)
end

type _ ty =
  | Int : int ty
  | Fun : 'a ty * 'b ty -> ('a -> 'b) ty
  | Vec : 'a ty -> 'a Vec.t ty

type dyn = Dyn : 'a ty * 'a -> dyn

let rec eq_ty : type a b. a ty -> b ty -> (a,b) eq option =
  fun t1 t2 -> match t1, t2 with
  | Int, Int -> Some Refl
  | Fun (t11, t12), Fun (t21, t22) ->
      begin match eq_ty t11 t21, eq_ty t12 t22 with
      | Some Refl, Some Refl -> Some Refl
      | _ -> None
      end
  | Vec t1, Vec t2 ->
      begin match eq_ty t1 t2 with
      | Some Refl -> Some Refl
      | None -> None
      end
  | _ -> None

let undyn : type a. a ty -> dyn -> a option =
  fun t1 (Dyn (t2, v)) ->
    match eq_ty t1 t2 with
    | Some Refl -> Some v
    | None -> None

let v = Vec.make 3 (fun n -> Vec.make n (fun m -> (m*n)))

let int_vec_vec = Vec (Vec Int)

let d = Dyn (int_vec_vec, v)

let Some v' = undyn int_vec_vec d
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module Vec :
  sig
    type +!'a t
    val make : int -> (int -> 'a) -> 'a t
    val get : 'a t -> int -> 'a
  end
type _ ty =
    Int : int ty
  | Fun : 'a ty * 'b ty -> ('a -> 'b) ty
  | Vec : 'a ty -> 'a Vec.t ty
type dyn = Dyn : 'a ty * 'a -> dyn
val eq_ty : 'a ty -> 'b ty -> ('a, 'b) eq option = <fun>
val undyn : 'a ty -> dyn -> 'a option = <fun>
val v : int Vec.t Vec.t = <abstr>
val int_vec_vec : int Vec.t Vec.t ty = Vec (Vec Int)
val d : dyn = Dyn (Vec (Vec Int), <poly>)
Line 47, characters 4-11:
47 | let Some v' = undyn int_vec_vec d
         ^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
None
val v' : int Vec.t Vec.t = <abstr>
|}]


(* Not directly related: injectivity and constraints *)
type 'a t = 'b constraint 'a = <b : 'b>
class type ['a] ct = object method m : 'b constraint 'a = < b : 'b > end
[%%expect{|
type 'a t = 'b constraint 'a = < b : 'b >
class type ['a] ct = object constraint 'a = < b : 'b > method m : 'b end
|}]

type _ u = M : 'a -> 'a t u (* OK *)
[%%expect{|
type _ u = M : < b : 'a > -> < b : 'a > t u
|}]
type _ v = M : 'a -> 'a ct v (* bug? *)
[%%expect{|
Line 1, characters 0-28:
1 | type _ v = M : 'a -> 'a ct v (* bug? *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]

type 'a t = 'b constraint 'a = <b : 'b; c : 'c>
type _ u = M : 'a -> 'a t u (* KO *)
[%%expect{|
type 'a t = 'b constraint 'a = < b : 'b; c : 'c >
Line 2, characters 0-27:
2 | type _ u = M : 'a -> 'a t u (* KO *)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, a type variable cannot be deduced
       from the type parameters.
|}]
