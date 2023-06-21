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
  let (Refl : (bool X.t, bool t) eq) as t = Obj.magic  () in ignore t
[%%expect{|
|}]
