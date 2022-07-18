(* TEST
 * expect *)

(* buildup *)
module type Ty = sig type t end
module F(T : Ty) = struct
  type t = Foo of (int -> T.t)
end
[%%expect {|
module type Ty = sig type t end
module F : functor (T : Ty) -> sig type t = Foo of (int -> T.t) end
|}]

(* this definition behaves weirdly *)
module rec M : sig type t = F(M).t end = F(M)
[%%expect {|
module rec M : sig type t = F(M).t end
|}]

(* intuitively this should be accepted, but is not *)
let _ : M.t = M.Foo (fun _ -> M.Foo (fun _ -> assert false))
[%%expect {|
Line 1, characters 14-19:
1 | let _ : M.t = M.Foo (fun _ -> M.Foo (fun _ -> assert false))
                  ^^^^^
Error: Unbound constructor M.Foo
|}]

(* .. but this is accepted -- using Foo from F(M) *)
let _ =
  let open F(M) in
  Foo (fun _ -> Foo (fun _ -> assert false))
[%%expect {|
- : F(M).t = F(M).Foo <fun>
|}]

(* ... or this -- binding F(M) to a new module *)
module N = F(M)
let _ : N.t = N.Foo (fun _ -> N.Foo (fun _ -> assert false))
[%%expect{|
module N : sig type t = F(M).t = Foo of (int -> M.t) end
- : N.t = N.Foo <fun>
|}]

(* ... or this -- exposing the variant definition in M directly *)
module rec M : sig type t = F(M).t = Foo of (int -> M.t) end = F(M)
let _ : M.t = M.Foo (fun _ -> M.Foo (fun _ -> assert false))
[%%expect{|
module rec M : sig type t = F(M).t = Foo of (int -> M.t) end
- : M.t = M.Foo <fun>
|}]
