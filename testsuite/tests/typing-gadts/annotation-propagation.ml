type _ t = T : int t;;
[%%expect{|
type _ t = T : int t
|}];;

let f : type a . a t * a -> unit = fun t ->
  let x : int = match t with T, n -> n in
  ignore (x + 1)
;;
[%%expect{|
val f : 'a t * 'a -> unit = <fun>
|}];;

let f : type a . a t * a -> unit = fun t ->
  let (_ as x) : int = match t with T, n -> n in
  ignore (x + 1)
;;
[%%expect{|
val f : 'a t * 'a -> unit = <fun>
|}];;

let f : type a . a t * a -> unit = fun t ->
  let x : type b . int = match t with T, n -> n in
  ignore (x + 1)
;;
[%%expect{|
val f : 'a t * 'a -> unit = <fun>
|}];;

(** ideally we would like this test to also pass under -principal,
    but this requires fixing several hurdles with the current
    type-checking of polymorphic annotations. *)
let f : type a . a t * a -> unit = fun t ->
  let x : 'b . int = match t with T, n -> n in
  ignore (x + 1)
;;
[%%expect{|
val f : 'a t * 'a -> unit = <fun>
|}, Principal{|
Line _, characters 42-43:
Error: This expression has type a but an expression was expected of type int
|}];;
