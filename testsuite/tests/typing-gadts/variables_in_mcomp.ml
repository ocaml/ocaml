module M = struct
  type 'a s = 'a
  type t = T : 'a s -> t
end

module N = struct
  type 'a s = 'a
  type t = T : 'a s -> t
end

type (_, _) eq = Refl : ('a, 'a) eq

let f (x : (M.t, N.t) eq)=
  match x with
  | Refl -> ()

[%%expect{|
module M : sig type 'a s = 'a type t = T : 'a s -> t end
module N : sig type 'a s = 'a type t = T : 'a s -> t end
type (_, _) eq = Refl : ('a, 'a) eq
val f : (M.t, N.t) eq -> unit = <fun>
|}]
