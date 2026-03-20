(* TEST
 expect;
*)

module rec M: sig
  type t = { r: 'a 'b. 'a -> ' b -> 'a }
  val f: t -> t
end = struct
  type t = { r : 'a. 'a -> 'a -> 'a }
  let f: t -> M.t = fun x -> x
end
[%%expect{|
Line 6, characters 29-30:
6 |   let f: t -> M.t = fun x -> x
                                 ^
Error: The value "x" has type "t" but an expression was expected of type "M.t"
|}]
