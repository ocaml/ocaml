(* TEST
 expect;
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
Line 2, characters 6-41:
2 |   let (Refl : (bool X.t, bool t) eq) as t = Obj.magic  () in ()
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 26 [unused-var]: unused variable t.
|}]
