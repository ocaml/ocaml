(* TEST
   * expect
*)

module X : sig
  type 'a t
end = struct
  type 'a t
end

type 'a t

type (_,_) eq = Refl : ('a,'a) eq
[%%expect{|
module X : sig type 'a t end
type 'a t
type (_, _) eq = Refl : ('a, 'a) eq
|}]

let () =
  let (Refl : (bool X.t, bool t) eq) as t = Obj.magic  () in ()
[%%expect{|
Line 2, characters 7-11:
2 |   let (Refl : (bool X.t, bool t) eq) as t = Obj.magic  () in ()
           ^^^^
Error: This pattern matches values of type (bool X.t, bool X.t) eq
       but a pattern was expected which matches values of type
         (bool X.t, bool t) eq
       Type bool X.t is not compatible with type bool t
|}]
