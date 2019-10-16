(* TEST
   * expect
*)

(* Definitions *)

type t [@@unique "foo"]
type a = A [@@unique "alf"]
type b = {b: int} [@@unique "bar"]
type ext = .. [@@unique]
[%%expect{|
type t [@@unique "foo"]
type a = A [@@unique "alf"]
type b = { b : int; } [@@unique "bar"]
type ext = .. [@@unique "ext"]
|}]

(* Re-exporting rules *)

module M = struct type a = A [@@unique "M.a"] end;;
[%%expect{|
module M : sig type a = A [@@unique "M.a"] end
|}]

(* we can abstract *)
module M1 : sig type a end = M;;
[%%expect{|
module M1 : sig type a end
|}]

(* we can abstract keeping identity *)
module M2 : sig type a [@@unique "M.a"] end = M;;
[%%expect{|
module M2 : sig type a [@@unique "M.a"] end
|}]

(* we cannot forget identity of concrete type *)
module M3 : sig type a = A end = M;;
[%%expect{|
Line 1, characters 33-34:
1 | module M3 : sig type a = A end = M;;
                                     ^
Error: Signature mismatch:
       Modules do not match:
         sig type a = M.a = A [@@unique "M.a"] end
       is not included in
         sig type a = A end
       Type declarations do not match:
         type a = M.a = A [@@unique "M.a"]
       is not included in
         type a = A
       Unique identifier "M.a" was removed without abstracting the datatype
|}]

(* we can export an abbreviation of a unique type as unique *)
module M4 : sig type float_array [@@unique "array"] end =
  struct type float_array = float array end
module M5 : sig type 'a my_array [@@unique "array"] end =
  struct type 'a my_array = 'a array end
[%%expect{|
module M4 : sig type float_array [@@unique "array"] end
module M5 : sig type 'a my_array [@@unique "array"] end
|}]

(* beware of injectivity *)
module M6 : sig type 'a my_array [@@unique "array"] end =
  struct type 'a my_array = float array end
[%%expect{|
Line 2, characters 2-43:
2 |   struct type 'a my_array = float array end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type 'a my_array = float array end
       is not included in
         sig type 'a my_array [@@unique "array"] end
       Type declarations do not match:
         type 'a my_array = float array
       is not included in
         type 'a my_array [@@unique "array"]
       Unique identifier "array" was not present in original declaration
|}]

(* Compatibility *)

type 'a list1 = [] | (::) of 'a * 'a list1
type 'a list2 = [] | (::) of 'a * 'a list2 [@@unique "list2"]
[%%expect{|
type 'a list1 = [] | (::) of 'a * 'a list1
type 'a list2 = [] | (::) of 'a * 'a list2 [@@unique "list2"]
|}]

type _ typ =
  | Int : int typ
  | Bool : bool typ
  | List : 'a typ -> 'a List.t typ
  | Array : 'a typ -> 'a array typ
  | Queue : 'a typ -> 'a Queue.t typ
  | Stack : 'a typ -> 'a Stack.t typ
  | List1 : 'a typ -> 'a list1 typ
  | List2 : 'a typ -> 'a list2 typ ;;
[%%expect{|
type _ typ =
    Int : int typ
  | Bool : bool typ
  | List : 'a typ -> 'a List.t typ
  | Array : 'a typ -> 'a array typ
  | Queue : 'a typ -> 'a Queue.t typ
  | Stack : 'a typ -> 'a Stack.t typ
  | List1 : 'a typ -> 'a list1 typ
  | List2 : 'a typ -> 'a list2 typ
|}]

let rec eq : type a. a typ -> a typ -> bool = fun t1 t2 ->
  match t1, t2 with
  | Int, Int -> true
  | Bool, Bool -> true
  | List a1, List a2 -> eq a1 a2
  | List1 a1, List1 a2 -> eq a1 a2
  | List2 a1, List2 a2 -> eq a1 a2
  | Array a1, Array a2 -> eq a1 a2
  | Queue a1, Queue a2 -> eq a1 a2
  | Stack a1, Stack a2 -> eq a1 a2;;
[%%expect{|
Lines 2-10, characters 2-34:
 2 | ..match t1, t2 with
 3 |   | Int, Int -> true
 4 |   | Bool, Bool -> true
 5 |   | List a1, List a2 -> eq a1 a2
 6 |   | List1 a1, List1 a2 -> eq a1 a2
 7 |   | List2 a1, List2 a2 -> eq a1 a2
 8 |   | Array a1, Array a2 -> eq a1 a2
 9 |   | Queue a1, Queue a2 -> eq a1 a2
10 |   | Stack a1, Stack a2 -> eq a1 a2..
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
(List _, List1 _)
val eq : 'a typ -> 'a typ -> bool = <fun>
|}]

(* list2 is incompatible with both list and list1 *)
let rec eq : type a. a typ -> a typ -> bool = fun t1 t2 ->
  match t1, t2 with
  | Int, Int -> true
  | Bool, Bool -> true
  | List a1, List a2 -> eq a1 a2
  | List a1, List1 a2 -> false
  | List1 a1, List a2 -> false
  | List1 a1, List1 a2 -> eq a1 a2
  | List2 a1, List2 a2 -> eq a1 a2
  | Array a1, Array a2 -> eq a1 a2
  | Queue a1, Queue a2 -> eq a1 a2
  | Stack a1, Stack a2 -> eq a1 a2;;
[%%expect{|
val eq : 'a typ -> 'a typ -> bool = <fun>
|}]

(* Typical example *)

module M : sig type b [@@unique "M.b"] end = struct
  module M1 = struct type a = A [@@unique "M.b"] end
  type b = M1.a
end;;
[%%expect{|
module M : sig type b [@@unique "M.b"] end
|}]

module M : sig type b = A [@@unique "M.b"] end = struct
  module M1 = struct type a = A [@@unique "M.b"] end
  type b = M1.a = A [@@unique "M.b"]
end;;
[%%expect{|
module M : sig type b = A [@@unique "M.b"] end
|}]

module M2 : sig type c [@@unique "M.b"] end = struct
  type c = C [@@unique "M.b"]
end;;
[%%expect{|
module M2 : sig type c [@@unique "M.b"] end
|}]

(* Private types *)

module M : sig type a [@@unique "M.a"] end = struct
  type t = T of int [@@unique "M.a"]
  type a = private t
end
[%%expect{|
module M : sig type a [@@unique "M.a"] end
|}]
