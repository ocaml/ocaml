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
