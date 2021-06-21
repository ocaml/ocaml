(* TEST
   * expect
*)

type _ t =
  | IntLit : int t
  | BoolLit : bool t
;;

[%%expect{|
type _ t = IntLit : int t | BoolLit : bool t
|}]

let trivial t =
  match t with
  | IntLit -> ()
  | BoolLit -> ()
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit -> ()
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let trivial_annotated (type a) (t : a t) =
  match t with
  | IntLit -> ()
  | BoolLit -> ()
;;

[%%expect{|
val trivial_annotated : 'a t -> unit = <fun>
|}]

let trivial_merged t =
  match t with
  | IntLit
  | BoolLit -> ()
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit -> ()
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let trivial_merged_annotated (type a) (t : a t) =
  match t with
  | IntLit
  | BoolLit -> ()
;;

[%%expect{|
val trivial_merged_annotated : 'a t -> unit = <fun>
|}]

let trivial_merged_annotated_under_tuple1 (type a) (t : a t) =
  match (3, t) with
  | _, (IntLit
       | BoolLit) -> ()
;;

[%%expect{|
val trivial_merged_annotated_under_tuple1 : 'a t -> unit = <fun>
|}]

let trivial_merged_annotated_under_tuple2 (type a) (tt : a t * a t) =
  match tt with
  | IntLit, (IntLit | BoolLit) -> ()
  | _ -> ()
;;

[%%expect{|
Line 3, characters 22-29:
3 |   | IntLit, (IntLit | BoolLit) -> ()
                          ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type a t
       Type bool is not compatible with type a = int
|}]

let trivial_merged_annotated_under_tuple2 (type a) (tt : a t * a t) =
  match tt with
  | (IntLit | BoolLit), IntLit -> ()
  | _ -> ()
;;

[%%expect{|
val trivial_merged_annotated_under_tuple2 : 'a t * 'a t -> unit = <fun>
|}]


let trivial_merged_annotated_under_array (type a) (t : a t array) =
  match t with
  | [| (IntLit | BoolLit); _ |] -> ()
  | [| _; _; (BoolLit | IntLit) |] -> ()
  | [| _; _; _; (BoolLit | IntLit) |]
  | [| _; _; _; (BoolLit | IntLit); _ |] -> ()
  | _ -> ()
;;

[%%expect{|
val trivial_merged_annotated_under_array : 'a t array -> unit = <fun>
|}]

let simple t a =
  match t, a with
  | IntLit, 3 -> ()
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit, true -> ()
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let simple_annotated (type a) (t : a t) (a : a) =
  match t, a with
  | IntLit, 3 -> ()
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
val simple_annotated : 'a t -> 'a -> unit = <fun>
|}]

let simple_merged t a =
  match t, a with
  | IntLit, 3
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit, true -> ()
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let simple_merged_ambi (type a) (t : a t) a =
  match t, a with
  | IntLit, (3 : a)
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
Line 4, characters 13-17:
4 |   | BoolLit, true -> ()
                 ^^^^
Error: This pattern matches values of type bool
       but a pattern was expected which matches values of type a = bool
       This instance of bool is ambiguous:
       it would escape the scope of its equation
|}]


let simple_merged_not_annotated_enough (type a) (t : a t) a =
  match t, a with
  | IntLit, 3
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
Line 4, characters 13-17:
4 |   | BoolLit, true -> ()
                 ^^^^
Error: This pattern matches values of type bool
       but a pattern was expected which matches values of type int
|}]


let simple_merged_annotated (type a) (t : a t) (a : a) =
  match t, a with
  | IntLit, 3
  | BoolLit, true -> ()
  | _, _ -> ()
;;

[%%expect{|
val simple_merged_annotated : 'a t -> 'a -> unit = <fun>
|}]

let simple_mega_merged_annotated (type a) (t : a t) (a : a) =
  match t, a with
  | IntLit, 3
  | BoolLit, true
  | _, _ -> ()
;;

[%%expect{|
val simple_mega_merged_annotated : 'a t -> 'a -> unit = <fun>
|}]

let simple_merged_annotated_return (type a) (t : a t) (a : a) =
  match t, a with
  | IntLit, (3 as x)
  | BoolLit, (true as x) -> ignore x
  | _, _ -> ()
;;

[%%expect{|
Line 3, characters 12-20:
3 |   | IntLit, (3 as x)
                ^^^^^^^^
Error: This pattern matches values of type int
       This instance of int is ambiguous:
       it would escape the scope of its equation
|}]

let simple_merged_annotated_return_annotated (type a) (t : a t) (a : a) =
  match t, a with
  | IntLit, ((3 : a) as x)
  | BoolLit, ((true : a) as x) -> ignore x
  | _, _ -> ()
;;

[%%expect{|
val simple_merged_annotated_return_annotated : 'a t -> 'a -> unit = <fun>
|}]

(* test more scenarios: when the or-pattern itself is not at toplevel but under
   other patterns. *)

let simple_merged_annotated_under_tuple (type a) (pair : a t * a) =
  match (), pair with
  | (), ( IntLit, 3
        | BoolLit, true) -> ()
  | _, _ -> ()
;;

[%%expect{|
val simple_merged_annotated_under_tuple : 'a t * 'a -> unit = <fun>
|}]

let simple_merged_annotated_under_arrays (type a) (pair : a t * a) =
  match [| [| pair |] |] with
  | [| _ ; [| ( IntLit, 3
              | BoolLit, true) |] |] -> ()
  | _ -> ()
;;

[%%expect{|
val simple_merged_annotated_under_arrays : 'a t * 'a -> unit = <fun>
|}]


let simple_merged_annotated_under_poly_variant (type a) (pair : a t * a) =
  match `Foo pair with
  | `Foo ( IntLit, 3
         | BoolLit, true ) -> ()
  | _ -> ()
;;

[%%expect{|
val simple_merged_annotated_under_poly_variant : 'a t * 'a -> unit = <fun>
|}]

let simple_merged_annotated_under_poly_variant_annotated (type a) pair =
  match (`Foo pair : [ `Foo of (a t * a) ]) with
  | `Foo ( IntLit, 3
         | BoolLit, true ) -> ()
  | _ -> ()
;;

[%%expect{|
val simple_merged_annotated_under_poly_variant_annotated : 'a t * 'a -> unit =
  <fun>
|}]

type 'a iref = { content : 'a; };;
[%%expect{|
type 'a iref = { content : 'a; }
|}]

let simple_merged_annotated_under_record (type a) (pair : a t * a) =
  match { content = pair } with
  | { content = ( IntLit, 3
                | BoolLit, true ) } -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_record : 'a t * 'a -> unit = <fun>
|}]

let simple_merged_annotated_under_mutable_record (type a) (pair : a t * a) =
  match { contents = pair } with
  | { contents = ( IntLit, 3
                 | BoolLit, true ) } -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_mutable_record : 'a t * 'a -> unit = <fun>
|}]

type 'a piref = { pcontent : 'b. 'a * 'b; };;
[%%expect{|
type 'a piref = { pcontent : 'b. 'a * 'b; }
|}]

let simple_merged_annotated_under_poly_record1 (type a) (r : (a t * a) piref) =
  match r with
  | { pcontent = ( IntLit, 3
                 | BoolLit, true ), _ } -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_poly_record1 : ('a t * 'a) piref -> unit =
  <fun>
|}]

let simple_merged_annotated_under_poly_record2 (type a) (r : (a t * a) piref) =
  match r with
  | { pcontent = ( (IntLit, 3), _
                 | (BoolLit, true), _ ) } -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_poly_record2 : ('a t * 'a) piref -> unit =
  <fun>
|}]

let simple_merged_annotated_under_constructor (type a) (pair : a t * a) =
  match Some pair with
  | Some ( IntLit, 3
         | BoolLit, true ) -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_constructor : 'a t * 'a -> unit = <fun>
|}]

type _ gadt_opt =
  | GNone : 'a gadt_opt
  | GSome : 'a -> 'a gadt_opt
;;
[%%expect{|
type _ gadt_opt = GNone : 'a gadt_opt | GSome : 'a -> 'a gadt_opt
|}]

let simple_merged_annotated_under_gadt_constructor (type a) (pair : a t * a) =
  match GSome pair with
  | GSome ( IntLit, 3
          | BoolLit, true ) -> ()
  | _ -> ()
;;
[%%expect{|
val simple_merged_annotated_under_gadt_constructor : 'a t * 'a -> unit =
  <fun>
|}]

(* back to simpler tests. *)

let noop t a =
  match t, a with
  | IntLit, x -> x
  | BoolLit, x -> x
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit, x -> x
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let noop_annotated (type a) (t : a t) (a : a) : a =
  match t, a with
  | IntLit, x -> x
  | BoolLit, x -> x
;;

[%%expect{|
val noop_annotated : 'a t -> 'a -> 'a = <fun>
|}]

let noop_merged t a =
  match t, a with
  | IntLit, x
  | BoolLit, x -> x
;;

[%%expect{|
Line 4, characters 4-11:
4 |   | BoolLit, x -> x
        ^^^^^^^
Error: This pattern matches values of type bool t
       but a pattern was expected which matches values of type int t
       Type bool is not compatible with type int
|}]

let noop_merged_annotated (type a) (t : a t) (a : a) : a =
  match t, a with
  | IntLit, x
  | BoolLit, x -> x
;;

[%%expect{|
val noop_merged_annotated : 'a t -> 'a -> 'a = <fun>
|}]

(***)

type _ t2 =
  | Int : int -> int t2
  | Bool : bool -> bool t2

[%%expect{|
type _ t2 = Int : int -> int t2 | Bool : bool -> bool t2
|}]

let trivial2 t2 =
  match t2 with
  | Int _ -> ()
  | Bool _ -> ()
;;

[%%expect{|
Line 4, characters 4-10:
4 |   | Bool _ -> ()
        ^^^^^^
Error: This pattern matches values of type bool t2
       but a pattern was expected which matches values of type int t2
       Type bool is not compatible with type int
|}]

let trivial2_annotated (type a) (t2 : a t2) =
  match t2 with
  | Int _ -> ()
  | Bool _ -> ()
;;

[%%expect{|
val trivial2_annotated : 'a t2 -> unit = <fun>
|}]

let trivial2_merged t2 =
  match t2 with
  | Int _
  | Bool _ -> ()
;;

[%%expect{|
Line 4, characters 4-10:
4 |   | Bool _ -> ()
        ^^^^^^
Error: This pattern matches values of type bool t2
       but a pattern was expected which matches values of type int t2
       Type bool is not compatible with type int
|}]

let trivial2_merged_annotated (type a) (t2 : a t2) =
  match t2 with
  | Int _
  | Bool _ -> ()
;;

[%%expect{|
val trivial2_merged_annotated : 'a t2 -> unit = <fun>
|}]


let extract t2 =
  match t2 with
  | Int _ -> x
  | Bool _ -> x
;;

[%%expect{|
Line 4, characters 4-10:
4 |   | Bool _ -> x
        ^^^^^^
Error: This pattern matches values of type bool t2
       but a pattern was expected which matches values of type int t2
       Type bool is not compatible with type int
|}]

let extract_annotated (type a) (t2 : a t2) : a =
  match t2 with
  | Int x -> x
  | Bool x -> x
;;

[%%expect{|
val extract_annotated : 'a t2 -> 'a = <fun>
|}]

let extract_merged t2 =
  match t2 with
  | Int x
  | Bool x -> x
;;

[%%expect{|
Line 4, characters 4-10:
4 |   | Bool x -> x
        ^^^^^^
Error: This pattern matches values of type bool t2
       but a pattern was expected which matches values of type int t2
       Type bool is not compatible with type int
|}]

let extract_merged_annotated (type a) (t2 : a t2) : a =
  match t2 with
  | Int x
  | Bool x -> x
;;


[%%expect{|
Lines 3-4, characters 4-10:
3 | ....Int x
4 |   | Bool x.....
Error: The variable x on the left-hand side of this or-pattern has type
       int but on the right-hand side it has type bool
|}]

let extract_merged_super_annotated (type a) (t2 : a t2) : a =
  match t2 with
  | Int (x : a)
  | Bool (x : a) -> x
;;

[%%expect{|
val extract_merged_super_annotated : 'a t2 -> 'a = <fun>
|}]

let extract_merged_too_lightly_annotated (type a) (t2 : a t2) : a =
  match t2 with
  | Int (x : a)
  | Bool x -> x
;;

[%%expect{|
Lines 3-4, characters 4-10:
3 | ....Int (x : a)
4 |   | Bool x.....
Error: The variable x on the left-hand side of this or-pattern has type
       a but on the right-hand side it has type bool
|}]

let extract_merged_super_lightly_annotated (type a) (t2 : a t2) =
  match t2 with
  | Int (x : a)
  | Bool (x : a) -> x
;;

[%%expect{|
val extract_merged_super_lightly_annotated : 'a t2 -> 'a = <fun>
|}]

let lambiguity (type a) (t2 : a t2) =
  match t2 with
  | Int ((_ : a) as x)
  | Bool (x : a) -> x
;;

[%%expect{|
val lambiguity : 'a t2 -> 'a = <fun>
|}]

let rambiguity (type a) (t2 : a t2) =
  match t2 with
  | Int (_ as x)
  | Bool ((_ : a) as x) -> x
;;

[%%expect{|
Lines 3-4, characters 4-23:
3 | ....Int (_ as x)
4 |   | Bool ((_ : a) as x).....
Error: The variable x on the left-hand side of this or-pattern has type
       int but on the right-hand side it has type a
|}]


(***)

type _ t3 =
  | A : int t3
  | B : int t3

[%%expect{|
type _ t3 = A : int t3 | B : int t3
|}]

(* This was always allowed as the branches can unify. *)
let not_annotated x =
  match x with
  | A | B -> 3
;;

[%%expect{|
val not_annotated : int t3 -> int = <fun>
|}]

let return_int (type a) (x : a t3) =
  match x with
  | A | B -> 3
;;

[%%expect{|
val return_int : 'a t3 -> int = <fun>
|}]

let return_a (type a) (x : a t3) : a =
  match x with
  | A | B -> 3 (* fails because the equation [a = int] doesn't escape any of
                  the branches of this or-pattern. *)
;;

[%%expect{|
Line 3, characters 13-14:
3 |   | A | B -> 3 (* fails because the equation [a = int] doesn't escape any of
                 ^
Error: This expression has type int but an expression was expected of type a
|}]

(* Making sure we don't break a frequent pattern of GADTs indexed by polymorphic
   variants, where or-patterns were already accepted under or-patterns. *)

type any = [ `A | `B | `C | `D | `E | `F ]

type voyel = [ `A | `E ]

type _ letter =
  | A : [< any > `A ] letter
  | B : [< any > `B ] letter
  | C : [< any > `C ] letter
  | D : [< any > `D ] letter
  | E : [< any > `E ] letter
  | F : [< any > `F ] letter

type _ cased =
  | Up : 'a letter -> ([< any ] as 'a) cased
  | Lo : 'a letter -> ([< any ] as 'a) cased

type gvoyel = voyel cased
type a = [ `A ] cased
;;
[%%expect{|
type any = [ `A | `B | `C | `D | `E | `F ]
type voyel = [ `A | `E ]
type _ letter =
    A : [< any > `A ] letter
  | B : [< any > `B ] letter
  | C : [< any > `C ] letter
  | D : [< any > `D ] letter
  | E : [< any > `E ] letter
  | F : [< any > `F ] letter
type _ cased =
    Up : 'a letter -> ([< any ] as 'a) cased
  | Lo : 'a letter -> ([< any ] as 'a) cased
type gvoyel = voyel cased
type a = [ `A ] cased
|}]

let gvoyel_of_a : a -> gvoyel = function
  | Up A | Lo A as a -> a
;;
[%%expect{|
val gvoyel_of_a : a -> gvoyel = <fun>
|}]

(* Some other illustrations of the issues with as-patterns. *)

let f_ok (type a) (t : a t) (a : bool iref) (b : a iref) =
  match t, a, b with
  | IntLit,  ({ content = true } as x), _
  | BoolLit,  _,                        ({ content = true} as x) -> ignore x
  | _, _, _ -> ()
;;
[%%expect{|
val f_ok : 'a t -> bool iref -> 'a iref -> unit = <fun>
|}]


let f_amb (type a) (t : a t) (a : bool ref) (b : a ref) =
  match t, a, b with
  | IntLit,  ({ contents = true } as x), _
  | BoolLit,  _,                        ({ contents = true} as x) -> ignore x
  | _, _, _ -> ()
;;
[%%expect{|
Lines 3-4, characters 4-65:
3 | ....IntLit,  ({ contents = true } as x), _
4 |   | BoolLit,  _,                        ({ contents = true} as x)............
Error: The variable x on the left-hand side of this or-pattern has type
         bool ref
       but on the right-hand side it has type a ref
       Type bool is not compatible with type a
|}]

(********************************************)

type t =
  | A : 'a -> t
  | B : 'a -> t
;;
[%%expect{|
type t = A : 'a -> t | B : 'a -> t
|}]

let f = function
  | A x
  | B x -> ignore x
;;
[%%expect{|
Line 2, characters 6-7:
2 |   | A x
          ^
Error: This pattern matches values of type $A_'a
       The type constructor $A_'a would escape its scope
|}]
