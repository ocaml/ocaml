(* TEST
   * expect
*)

(*
   Implicit unpack allows the signature in (val ...) expressions to be omitted.

   It also adds (module M : S) and (module M) patterns, relying on
   implicit (val ...) for the implementation. Such patterns can only
   be used in function definition, match clauses, and let ... in.

   New: implicit pack is also supported, and you only need to be able
   to infer the the module type path from the context.
 *)
(* ocaml -principal *)

(* Use a module pattern *)
let sort (type s) (module Set : Set.S with type elt = s) l =
  Set.elements (List.fold_right Set.add l Set.empty)
;;
[%%expect{|
val sort : (module Set.S with type elt = 's) -> 's list -> 's list = <fun>
|}];;

(* No real improvement here? *)
let make_set (type s) cmp : (module Set.S with type elt = s) =
  (module Set.Make (struct type t = s let compare = cmp end))
;;
[%%expect{|
val make_set : ('s -> 's -> int) -> (module Set.S with type elt = 's) = <fun>
|}];;

(* No type annotation here *)
let sort_cmp (type s) cmp =
  sort (module Set.Make (struct type t = s let compare = cmp end))
;;
[%%expect{|
val sort_cmp : ('s -> 's -> int) -> 's list -> 's list = <fun>
|}];;

module type S = sig type t val x : t end;;
[%%expect{|
module type S = sig type t val x : t end
|}];;

let f (module M : S with type t = int) = M.x;;
[%%expect{|
val f : (module S with type t = int) -> int = <fun>
|}];;

let f (module M : S with type t = 'a) = M.x;; (* Error *)
[%%expect{|
Line 1, characters 14-15:
1 | let f (module M : S with type t = 'a) = M.x;; (* Error *)
                  ^
Error: The type of this packed module contains variables:
       (module S with type t = 'a)
|}];;

let f (type a) (module M : S with type t = a) = M.x;;
f (module struct type t = int let x = 1 end);;
[%%expect{|
val f : (module S with type t = 'a) -> 'a = <fun>
- : int = 1
|}];;

(***)

type 'a s = {s: (module S with type t = 'a)};;
[%%expect{|
type 'a s = { s : (module S with type t = 'a); }
|}];;

{s=(module struct type t = int let x = 1 end)};;
[%%expect{|
- : int s = {s = <module>}
|}];;

let f {s=(module M)} = M.x;; (* Error *)
[%%expect{|
Line 1, characters 9-19:
1 | let f {s=(module M)} = M.x;; (* Error *)
             ^^^^^^^^^^
Error: The type of this packed module contains variables:
       (module S with type t = 'a)
|}];;

let f (type a) ({s=(module M)} : a s) = M.x;;
[%%expect{|
val f : 'a s -> 'a = <fun>
|}];;

type s = {s: (module S with type t = int)};;
let f {s=(module M)} = M.x;;
let f {s=(module M)} {s=(module N)} = M.x + N.x;;
[%%expect{|
type s = { s : (module S with type t = int); }
val f : s -> int = <fun>
val f : s -> s -> int = <fun>
|}];;

(***)

module type S = sig val x : int end;;
[%%expect{|
module type S = sig val x : int end
|}];;

let f (module M : S) y (module N : S) = M.x + y + N.x;;
[%%expect{|
val f : (module S) -> int -> (module S) -> int = <fun>
|}];;

let m = (module struct let x = 3 end);; (* Error *)
[%%expect{|
Line 1, characters 8-37:
1 | let m = (module struct let x = 3 end);; (* Error *)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The signature for this packaged module couldn't be inferred.
|}];;

let m = (module struct let x = 3 end : S);;
[%%expect{|
val m : (module S) = <module>
|}];;

f m 1 m;;
[%%expect{|
- : int = 7
|}];;
f m 1 (module struct let x = 2 end);;
[%%expect{|
- : int = 6
|}];;

(***)

let (module M) = m in M.x;;
[%%expect{|
- : int = 3
|}];;

let (module M) = m;; (* Error: only allowed in [let .. in] *)
[%%expect{|
Line 1, characters 4-14:
1 | let (module M) = m;; (* Error: only allowed in [let .. in] *)
        ^^^^^^^^^^
Error: Modules are not allowed in this pattern.
|}];;

class c = let (module M) = m in object end;; (* Error again *)
[%%expect{|
Line 1, characters 14-24:
1 | class c = let (module M) = m in object end;; (* Error again *)
                  ^^^^^^^^^^
Error: Modules are not allowed in this pattern.
|}];;

module M = (val m);;
[%%expect{|
module M : S
|}];;

(***)

module type S' = sig val f : int -> int end;;
[%%expect{|
module type S' = sig val f : int -> int end
|}];;

(* Even works with recursion, but must be fully explicit *)
let rec (module M : S') =
  (module struct let f n = if n <= 0 then 1 else n * M.f (n-1) end : S')
in M.f 3;;
[%%expect{|
- : int = 6
|}];;

(* Subtyping *)

module type S = sig type t type u val x : t * u end

let f (l : (module S with type t = int and type u = bool) list) =
  (l :> (module S with type u = bool) list)
;;
[%%expect{|
module type S = sig type t type u val x : t * u end
val f :
  (module S with type t = int and type u = bool) list ->
  (module S with type u = bool) list = <fun>
|}];;

(* GADTs from the manual *)
(* the only modification is in to_string *)

module TypEq : sig
  type ('a, 'b) t
  val apply: ('a, 'b) t -> 'a -> 'b
  val refl: ('a, 'a) t
  val sym: ('a, 'b) t -> ('b, 'a) t
end = struct
  type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
  let refl = (fun x -> x), (fun x -> x)
  let apply (f, _) x = f x
  let sym (f, g) = (g, f)
end

module rec Typ : sig
  module type PAIR = sig
    type t and t1 and t2
    val eq: (t, t1 * t2) TypEq.t
    val t1: t1 Typ.typ
    val t2: t2 Typ.typ
  end

  type 'a typ =
    | Int of ('a, int) TypEq.t
    | String of ('a, string) TypEq.t
    | Pair of (module PAIR with type t = 'a)
end = Typ

let int = Typ.Int TypEq.refl

let str = Typ.String TypEq.refl

let pair (type s1) (type s2) t1 t2 =
  let module P = struct
    type t = s1 * s2
    type t1 = s1
    type t2 = s2
    let eq = TypEq.refl
    let t1 = t1
    let t2 = t2
  end in
  Typ.Pair (module P)

open Typ
let rec to_string: 'a. 'a Typ.typ -> 'a -> string =
  fun (type s) t x ->
    match (t : s typ) with
    | Int eq -> Int.to_string (TypEq.apply eq x)
    | String eq -> Printf.sprintf "%S" (TypEq.apply eq x)
    | Pair (module P) ->
        let (x1, x2) = TypEq.apply P.eq x in
        Printf.sprintf "(%s,%s)" (to_string P.t1 x1) (to_string P.t2 x2)
;;
[%%expect{|
module TypEq :
  sig
    type ('a, 'b) t
    val apply : ('a, 'b) t -> 'a -> 'b
    val refl : ('a, 'a) t
    val sym : ('a, 'b) t -> ('b, 'a) t
  end
module rec Typ :
  sig
    module type PAIR =
      sig
        type t
        and t1
        and t2
        val eq : (t, t1 * t2) TypEq.t
        val t1 : t1 Typ.typ
        val t2 : t2 Typ.typ
      end
    type 'a typ =
        Int of ('a, int) TypEq.t
      | String of ('a, string) TypEq.t
      | Pair of (module PAIR with type t = 'a)
  end
val int : int Typ.typ = Typ.Int <abstr>
val str : string Typ.typ = Typ.String <abstr>
val pair : 's1 Typ.typ -> 's2 Typ.typ -> ('s1 * 's2) Typ.typ = <fun>
val to_string : 'a Typ.typ -> 'a -> string = <fun>
|}];;

(* Wrapping maps *)
module type MapT = sig
  include Map.S
  type data
  type map
  val of_t : data t -> map
  val to_t : map -> data t
end

type ('k,'d,'m) map =
    (module MapT with type key = 'k and type data = 'd and type map = 'm)

let add (type k) (type d) (type m) (m:(k,d,m) map) x y s =
   let module M =
     (val m:MapT with type key = k and type data = d and type map = m) in
   M.of_t (M.add x y (M.to_t s))

module SSMap = struct
  include Map.Make(String)
  type data = string
  type map = data t
  let of_t x = x
  let to_t x = x
end
;;
[%%expect{|
module type MapT =
  sig
    type key
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    type data
    type map
    val of_t : data t -> map
    val to_t : map -> data t
  end
type ('k, 'd, 'm) map =
    (module MapT with type data = 'd and type key = 'k and type map = 'm)
val add : ('k, 'd, 'm) map -> 'k -> 'd -> 'm -> 'm = <fun>
module SSMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
    type data = string
    type map = data t
    val of_t : 'a -> 'a
    val to_t : 'a -> 'a
  end
|}];;

let ssmap =
  (module SSMap:
   MapT with type key = string and type data = string and type map = SSMap.map)
;;
[%%expect{|
val ssmap :
  (module MapT with type data = string and type key = string and type map =
   SSMap.map) =
  <module>
|}];;

let ssmap =
  (module struct include SSMap end :
   MapT with type key = string and type data = string and type map = SSMap.map)
;;
[%%expect{|
val ssmap :
  (module MapT with type data = string and type key = string and type map =
   SSMap.map) =
  <module>
|}];;

let ssmap =
  (let module S = struct include SSMap end in (module S) :
  (module
   MapT with type key = string and type data = string and type map = SSMap.map))
;;
[%%expect{|
val ssmap :
  (module MapT with type data = string and type key = string and type map =
   SSMap.map) =
  <module>
|}];;

let ssmap =
  (module SSMap: MapT with type key = _ and type data = _ and type map = _)
;;
[%%expect{|
val ssmap :
  (module MapT with type data = SSMap.data and type key = SSMap.key and type map =
   SSMap.map) =
  <module>
|}];;

let ssmap : (_,_,_) map = (module SSMap);;
[%%expect{|
val ssmap : (SSMap.key, SSMap.data, SSMap.map) map = <module>
|}];;

add ssmap;;
[%%expect{|
- : SSMap.key -> SSMap.data -> SSMap.map -> SSMap.map = <fun>
|}];;

(*****)

module type S = sig type t end

let x =
  (module struct type elt = A type t = elt list end : S with type t = _ list)
;;
[%%expect{|
module type S = sig type t end
Line 4, characters 10-51:
4 |   (module struct type elt = A type t = elt list end : S with type t = _ list)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type t in this module cannot be exported.
       Its type contains local dependencies: elt list
|}];;

type 'a s = (module S with type t = 'a);;
[%%expect{|
type 'a s = (module S with type t = 'a)
|}];;

let x : 'a s = (module struct type t = int end);;
[%%expect{|
val x : int s = <module>
|}];;

let x : 'a s = (module struct type t = A end);;
[%%expect{|
Line 1, characters 23-44:
1 | let x : 'a s = (module struct type t = A end);;
                           ^^^^^^^^^^^^^^^^^^^^^
Error: The type t in this module cannot be exported.
       Its type contains local dependencies: t
|}];;

let x : 'a s = (module struct end);;
[%%expect{|
Line 1, characters 23-33:
1 | let x : 'a s = (module struct end);;
                           ^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig end is not included in S
       The type `t' is required but not provided
|}];;
