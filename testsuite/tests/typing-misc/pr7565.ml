(** Test that object type are expanded in error message, when the error
     message stems from an escaping universal type variable, cf MPR#7565 *)

type t_a = <f: 'a. 'a -> int >
let f (o:t_a) = o # f 0

let () =
  let o =
    object
      method f _ = 0
    end
  in
  f o;;
[%%expect{|
type t_a = < f : 'a. 'a -> int >
val f : t_a -> int = <fun>
Line _, characters 4-5:
Error: This expression has type < f : 'b -> int >
       but an expression was expected of type t_a = < f : 'a. 'a -> int >
       The universal variable 'a would escape its scope
|}];;

type uv = [ `A of <f: 'a. 'a -> int > ]
let f (`A o:uv) = o # f 0

let () =
  let o =
    `A (object
      method f _ = 0
    end)
  in
  f o;;
[%%expect{|
type uv = [ `A of < f : 'a. 'a -> int > ]
val f : uv -> int = <fun>
Line _, characters 4-5:
Error: This expression has type [> `A of < f : 'b -> int > ]
       but an expression was expected of type
         uv = [ `A of < f : 'a. 'a -> int > ]
       The universal variable 'a would escape its scope
|}];;

type 'a t = <f:'a -> int>
let f (o:t_a) = o # f 0

let () =
  let o: _ t =
    object
      method f _ = 0
    end
  in
  f o;;
[%%expect{|
type 'a t = < f : 'a -> int >
val f : t_a -> int = <fun>
Line _, characters 4-5:
Error: This expression has type 'b t = < f : 'b -> int >
       but an expression was expected of type t_a = < f : 'a. 'a -> int >
       The universal variable 'a would escape its scope
|}]
