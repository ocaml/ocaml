(* TEST
 expect;
*)

(** Test that as-patterns let us re-specialize the type of a constructor packing an existential *)

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
