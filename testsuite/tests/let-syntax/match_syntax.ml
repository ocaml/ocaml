(* TEST
   * expect
*)

module List = struct

  let concat_map f l =
    let l = List.map f l in
    List.concat l

  let product xs ys =
    List.fold_right
      (fun x acc -> (List.map (fun y -> (x, y)) ys) @ acc)
      xs []

  let (match+) = List.map

  let ( match* ) = concat_map

end;;
[%%expect{|
module List :
  sig
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list
    val product : 'a list -> 'b list -> ('a * 'b) list
    val ( match+ ) : ('a -> 'b) -> 'a list -> 'b list
    val ( match* ) : ('a -> 'b list) -> 'a list -> 'b list
  end
|}];;

let match_map =
  List.(
    match+ [1; 2; 3] with
    | 2 -> true
    | _ -> false
  );;
[%%expect{|
val match_map : bool list = [false; true; false]
|}];;

let match_bind =
  List.(
    match* [1; 2; 3] with
    | 2 -> [true; true; true]
    | _ -> [false]
  );;
[%%expect{|
val match_bind : bool list = [false; true; true; true; false]
|}];;

module Ill_typed_1 = struct

  let (match+) = fun f x -> not x

end;;
[%%expect{|
module Ill_typed_1 : sig val ( match+ ) : 'a -> bool -> bool end
|}];;

let ill_typed_1 =
  Ill_typed_1.(
    match+ 1 with
    | _ -> ()
  );;
[%%expect{|
Line 3, characters 11-12:
      match+ 1 with
             ^
Error: This expression has type int but an expression was expected of type
         bool
|}];;

module Ill_typed_2 = struct

  let (match+) = 5

end;;
[%%expect{|
module Ill_typed_2 : sig val ( match+ ) : int end
|}];;

let ill_typed_2 =
  Ill_typed_2.(
    match+ 2 with
    | _ -> ()
  );;
[%%expect{|
Line 3, characters 4-10:
      match+ 2 with
      ^^^^^^
Error: This expression has type int
       This is not a function; it cannot be applied.
|}];;

module Ill_typed_3 = struct

  let (match+) = fun f x -> not f

end;;
[%%expect{|
module Ill_typed_3 : sig val ( match+ ) : bool -> 'a -> bool end
|}];;

let ill_typed_3 =
  Ill_typed_3.(
    match+ 2 with
    | _ -> ()
  );;
[%%expect{|
Line 3, characters 4-31:
  ....match+ 2 with
      | _ -> ()
Error: This expression should not be a function, the expected type is
bool
|}];;
