(* TEST
 expect;
*)

(** Test that as-patterns let us re-specialize the type of a constructor packing
    an existential *)

(* No payload *)
type 'a t =
  | Left : [> `Left ] t
  | Right : [> `Right ] t
[%%expect {|
type 'a t = Left : [> `Left ] t | Right : [> `Right ] t
|}]

let left : [ `Left | `Right ] t -> [ `Left ] t = function
  | Left as t -> t
  | Right -> assert false
[%%expect {|
val left : [ `Left | `Right ] t -> [ `Left ] t = <fun>
|}]

(* Concrete payload *)
type ('a, 'e) t =
  | Left : 'e -> ([> `Left ], 'e) t
  | Right : 'e -> ([> `Right ], 'e) t
[%%expect {|
type ('a, 'e) t =
    Left : 'e -> ([> `Left ], 'e) t
  | Right : 'e -> ([> `Right ], 'e) t
|}]

let left : ([ `Left | `Right ], 'e) t -> ([ `Left ], 'e) t = function
  | Left _ as t -> t
  | Right _ -> assert false
[%%expect {|
val left : ([ `Left | `Right ], 'e) t -> ([ `Left ], 'e) t = <fun>
|}]

(* Pack payload *)
type 'a t2 = P : ('a, 'e) t -> 'a t2 [@@unboxed]
[%%expect {|
type 'a t2 = P : ('a, 'e) t -> 'a t2 [@@unboxed]
|}]

let left : [ `Left | `Right ] t2 -> [ `Left ] t2 = function
  | P (Left _ as t) -> P t
  | P (Right _) -> assert false
[%%expect {|
val left : [ `Left | `Right ] t2 -> [ `Left ] t2 = <fun>
|}]

(* Existential payload - equivalent to packed concrete payload *)
type 'a t =
  | Left : 'e -> [> `Left ] t
  | Right : 'e -> [> `Right ] t
[%%expect {|
type 'a t = Left : 'e -> [> `Left ] t | Right : 'e -> [> `Right ] t
|}]

let left : [ `Left | `Right ] t -> [ `Left ] t = function
  | Left _ as t -> t
  | Right _ -> assert false
[%%expect {|
val left : [ `Left | `Right ] t -> [ `Left ] t = <fun>
|}]

(* Some examples require more work *)
type 'a boxed_int = private int

(* Used to work, and should still work *)
type t =
  | Value_int of int
  | Value_boxed_int : 'a boxed_int -> t

let f (x : t) : t =
  match x with
  | Value_int _ | Value_boxed_int _ as y -> y;;
[%%expect{|
type 'a boxed_int = private int
type t = Value_int of int | Value_boxed_int : 'a boxed_int -> t
val f : t -> t = <fun>
|}]

(* Expected typing, an easy case: no existentials *)
type 'a good_t = Val of 'a | Boxed : unit boxed_int -> 'a good_t | Other

let f = function
  | Val x -> Val true
  | (Boxed _ | Other) as y -> y;;
[%%expect{|
type 'a good_t = Val of 'a | Boxed : unit boxed_int -> 'a good_t | Other
val f : 'a good_t -> bool good_t = <fun>
|}]

(* Requires to unify existentials *)
type 'a bad_t = Val of 'a | Boxed : 'b boxed_int -> 'a bad_t | Other
let f = function
  | Val x -> Val true
  | (Boxed _ | Other) as y -> y;;
[%%expect{|
type 'a bad_t = Val of 'a | Boxed : 'b boxed_int -> 'a bad_t | Other
val f : 'a bad_t -> bool bad_t = <fun>
|}]

(* Example with an existential at a higher level *)
type _ t =
  | Value_int of int
  | Value_snd : ('a,'b option) result -> 'b t;;

(*
  level(f)
< level(match x ..) = level(.. as y)
< level(Value_snd ..) (an existential $a is introduced)
< level(Ok ..) ($a is used)

(Ok _ | Error None) : ($a,'b option) result
*)
let f (x : _ t) =
  match x with
  | (Value_int _ | Value_snd (Ok _ | Error None)) as y -> y
  | Value_snd (Error (Some _)) -> Value_snd (Error None);;
[%%expect{|
type _ t = Value_int of int | Value_snd : ('a, 'b option) result -> 'b t
val f : 'a t -> 'b t = <fun>
|}]
