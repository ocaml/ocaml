(* TEST
   * expect
*)

module M = struct
  type _ rr = Soa : int rr
  type b = B : 'a rr * 'a -> b
end

let test =
  let M.(B (k, v)) = M.(B (Soa, 0)) in
  match k, v with
  | M.Soa, soa -> (soa : int)
[%%expect{|
module M : sig type _ rr = Soa : int rr type b = B : 'a rr * 'a -> b end
val test : int = 0
|}]

let test =
  let open M in
  let B (k, v) = B (Soa, 0) in
  match k, v with
  | Soa, soa -> (soa : int)
[%%expect{|
val test : int = 0
|}]

type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
[%%expect{|
type _ ty = Int : int ty
type dyn = Dyn : 'a ty * 'a -> dyn
|}]

let f String.(Dyn (type a) (w, x : a ty * a)) = ignore (x : a)
[%%expect{|
val f : dyn -> unit = <fun>
|}]
