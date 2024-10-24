(* TEST
   expect;
*)

(* We expect this to work. *)
type t = A [@@shape [imm 0]]
[%%expect {|
type t = A
|}] (* expected result *)

(* We expect this to fail, the shape annotation is too restrictive. *)
type t = A | B [@@shape [imm 0]]
[%%expect {|
type t = A | B
|}] (* unexpected result *)

(* We expect this to fail, the shape annotation is too restrictive. *)
type t = int [@@shape [imm 0]]
[%%expect {|
type t = int
|}] (* unexpected result *)


(* Buildup for further tests. *)
module type ImmOrFun = sig
  type t [@@shape [int; \#function]]
end
[%%expect {|
module type ImmOrFun = sig type t end
|}]

(* We expect this to work:
   the implementation has a smaller shape than the signature. *)
module Valid : ImmOrFun = struct
  type t = bool
end
[%%expect {|
module Valid : ImmOrFun
|}]

(* We expect this to fail: the implementation does not match the signature *)
module Invalid : ImmOrFun = struct
  type t = int list
end
[%%expect {|
module Invalid : ImmOrFun
|}] (* unexpected result *)

(* This functor declaration should be accepted *)
module Valid (T : ImmOrFun) = struct
  type t = T.t

  type u = t [@@shape [int; \#function; array]]

  type other =
    | ImmOrFun of T.t [@unboxed]
    | String of string [@unboxed]
end
[%%expect {|
module Valid :
  (T : ImmOrFun) ->
    sig
      type t = T.t
      type u = t
      type other = ImmOrFun of T.t [@unboxed] | String of string [@unboxed]
    end
|}] (* expected result *)

module Invalid (T : ImmOrFun) = struct
  type t = T.t [@@shape string]
end
[%%expect {|
module Invalid : (T : ImmOrFun) -> sig type t = T.t end
|}] (* unexpected result *)

(* This functor application should be rejected. *)
module InvalidApplication = Valid(struct type t = string end)
[%%expect {|
module InvalidApplication :
  sig
    type t = string
    type u = t
    type other = ImmOrFun of string [@unboxed] | String of string [@unboxed]
  end
|}] (* unexpected result *)

(* If we apply the functor to a type that is narrower than the signature,
   do we get back a narrow type? *)
module ValidApplication = Valid(struct type t = int (* not function *) end)
type t = T of ValidApplication.t [@unboxed] | Function of (int -> int) [@unboxed]
[%%expect {|
module ValidApplication :
  sig
    type t = int
    type u = t
    type other = ImmOrFun of int [@unboxed] | String of string [@unboxed]
  end
type t =
    T of ValidApplication.t [@unboxed]
  | Function of (int -> int) [@unboxed]
|}]

