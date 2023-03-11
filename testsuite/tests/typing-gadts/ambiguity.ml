(* TEST
   * expect
*)

[@@@warning "-8-11-12"] (* reduce the noise. *)

type ('a, 'b) eq = Refl : ('a, 'a) eq;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
|}];;

let ret_e1 (type a b) (b : bool) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> if b then x else y
  | _ -> x
;;
[%%expect{|
Line 3, characters 29-30:
3 |   | Refl -> if b then x else y
                                 ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let ret_e2 (type a b) (b : bool) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> if b then x else y
  | _ -> y
;;
[%%expect{|
Line 3, characters 29-30:
3 |   | Refl -> if b then x else y
                                 ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let ret_ei1 (type a) (b : bool) (wit : (a, int) eq) (x : a) =
  match wit with
  | Refl -> if b then x else 0
  | _ -> x
;;
[%%expect{|
Line 3, characters 29-30:
3 |   | Refl -> if b then x else 0
                                 ^
Error: This expression has type int but an expression was expected of type
         a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

let ret_ei2 (type a) (b : bool) (wit : (a, int) eq) (x : a) =
  match wit with
  | Refl -> if b then x else 0
  | _ -> x
;;
[%%expect{|
Line 3, characters 29-30:
3 |   | Refl -> if b then x else 0
                                 ^
Error: This expression has type int but an expression was expected of type
         a = int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]


let ret_f (type a b) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> [x; y]
  | _ -> [x]
;;
[%%expect{|
Line 3, characters 16-17:
3 |   | Refl -> [x; y]
                    ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let ret_g1 (type a b) (wit : (a, b) eq) (x : a) (y : b) =
  match wit with
  | Refl -> [x; y]
  | _ -> [y]
;;
[%%expect{|
Line 3, characters 16-17:
3 |   | Refl -> [x; y]
                    ^
Error: This expression has type b = a but an expression was expected of type
         a
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

(* First reported in MPR#7617: the typechecker arbitrarily picks a
   representative for an ambivalent type escaping its scope.
   The commit that was implemented poses problems of its own: we are now
   unifying the type of the patterns in the environment of each pattern, instead
   of the outer one. The code discussed in PR#7617 passes because each branch
   contains the same equation, but consider the following cases: *)

let f (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
  | _, [(_ : a)] -> []
;;
[%%expect{|
Line 3, characters 4-29:
3 |   | Refl, [(_ : a) | (_ : b)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * a list
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let g1 (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : a) | (_ : b)] -> []
  | _, [(_ : b)] -> []
;;
[%%expect{|
Line 3, characters 4-29:
3 |   | Refl, [(_ : a) | (_ : b)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * a list
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let g2 (type a b) (x : (a, b) eq) =
  match x, [] with
  | Refl, [(_ : b) | (_ : a)] -> []
  | _, [(_ : a)] -> []
;;
[%%expect{|
Line 3, characters 4-29:
3 |   | Refl, [(_ : b) | (_ : a)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

let h1 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : a)] -> []
  | Refl, [(_ : a) | (_ : b)] -> []
;;
[%%expect{|
Line 4, characters 4-29:
4 |   | Refl, [(_ : a) | (_ : b)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * a list
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let h2 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : b)] -> []
  | Refl, [(_ : a) | (_ : b)] -> []
;;
[%%expect{|
Line 4, characters 4-29:
4 |   | Refl, [(_ : a) | (_ : b)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * a list
       This instance of a is ambiguous:
       it would escape the scope of its equation
|}]

let h3 (type a b) (x : (a, b) eq) =
  match x, [] with
  | _, [(_ : a)] -> []
  | Refl, [(_ : b) | (_ : a)] -> []
;;
[%%expect{|
Line 4, characters 4-29:
4 |   | Refl, [(_ : b) | (_ : a)] -> []
        ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern matches values of type (a, b) eq * b list
       This instance of b is ambiguous:
       it would escape the scope of its equation
|}]

module T : sig
  type t
  type u
  val eq : (t, u) eq
end = struct
  type t = int
  type u = int
  let eq = Refl
end;;
[%%expect{|
module T : sig type t type u val eq : (t, u) eq end
|}]

module M = struct
  let r = ref []
end

let foo p (e : (T.t, T.u) eq) (x : T.t) (y : T.u) =
  match e with
  | Refl ->
    let z = if p then x else y in
    let module N = struct
      module type S = module type of struct let r = ref [z] end
    end in
    let module O : N.S = M in
    ()

module type S = module type of M ;;
[%%expect{|
module M : sig val r : '_weak1 list ref end
Line 12, characters 25-26:
12 |     let module O : N.S = M in
                              ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak1 list ref end
       is not included in
         N.S
       Values do not match:
         val r : '_weak1 list ref
       is not included in
         val r : T.t list ref
       The type '_weak1 list ref is not compatible with the type T.t list ref
       This instance of T.t is ambiguous:
       it would escape the scope of its equation
|}]

module M = struct
  let r = ref []
end

let foo p (e : (T.u, T.t) eq) (x : T.t) (y : T.u) =
  match e with
  | Refl ->
    let z = if p then x else y in
    let module N = struct
      module type S = module type of struct let r = ref [z] end
    end in
    let module O : N.S = M in
    ()

module type S = module type of M ;;
[%%expect{|
module M : sig val r : '_weak2 list ref end
Line 12, characters 25-26:
12 |     let module O : N.S = M in
                              ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak2 list ref end
       is not included in
         N.S
       Values do not match:
         val r : '_weak2 list ref
       is not included in
         val r : T.t list ref
       The type '_weak2 list ref is not compatible with the type T.t list ref
       Type '_weak2 is not compatible with type T.t = T.u
       This instance of T.u is ambiguous:
       it would escape the scope of its equation
|}]
