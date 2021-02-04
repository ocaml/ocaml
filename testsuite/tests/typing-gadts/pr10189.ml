(* TEST
   * expect
*)

type i = <m : 'c. 'c -> 'c >
type ('a, 'b) j = <m : 'c. 'a -> 'b >
type _ t = A : i t;;
[%%expect{|
type i = < m : 'c. 'c -> 'c >
type ('a, 'b) j = < m : 'a -> 'b >
type _ t = A : i t
|}]

let f (type a b) (y : (a, b) j t) : a -> b =
  let A = y in fun x -> x;;
[%%expect{|
Line 2, characters 6-7:
2 |   let A = y in fun x -> x;;
          ^
Error: This pattern matches values of type i t
       but a pattern was expected which matches values of type (a, b) j t
       Type i = < m : 'c. 'c -> 'c > is not compatible with type
         (a, b) j = < m : a -> b >
       The method m has type 'c. 'c -> 'c, but the expected method type was
       a -> b
       The universal variable 'c would escape its scope
|}]

let g (type a b) (y : (a,b) j t option) =
  let None = y in () ;;
[%%expect{|
val g : ('a, 'b) j t option -> unit = <fun>
|}]

(* more examples by @lpw25 *)
module M = struct
  type a
  type i = C of <m : 'c. 'c -> 'c >
  type j = C of <m : 'c. 'c -> a >
end
type _ t = A : M.i t;;
let f (y : M.j t) = match y with _ -> .;;
[%%expect{|
module M :
  sig
    type a
    type i = C of < m : 'c. 'c -> 'c >
    type j = C of < m : 'c. 'c -> a >
  end
type _ t = A : M.i t
val f : M.j t -> 'a = <fun>
|}]

module M = struct
  type a
  type i = C of <m : 'c. 'c -> 'c -> 'c >
  type j = C of <m : 'c. 'c -> a >
end
type _ t = A : M.i t;;
let f (y : M.j t) = match y with _ -> .;;
[%%expect{|
module M :
  sig
    type a
    type i = C of < m : 'c. 'c -> 'c -> 'c >
    type j = C of < m : 'c. 'c -> a >
  end
type _ t = A : M.i t
val f : M.j t -> 'a = <fun>
|}]

module M = struct
  type 'a a
  type i = C of <m : 'c. 'c -> 'c -> 'c >
  type j = C of <m : 'c. 'c -> 'c a >
end
type _ t = A : M.i t;;
let f (y : M.j t) = match y with _ -> .;;
[%%expect{|
module M :
  sig
    type 'a a
    type i = C of < m : 'c. 'c -> 'c -> 'c >
    type j = C of < m : 'c. 'c -> 'c a >
  end
type _ t = A : M.i t
Line 7, characters 33-34:
7 | let f (y : M.j t) = match y with _ -> .;;
                                     ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: A
|}]
