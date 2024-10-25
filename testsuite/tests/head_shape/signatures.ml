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
Line 1, characters 0-32:
1 | type t = A | B [@@shape [imm 0]]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this type declaration, the actual head shape does not match the
       expected type shape.
|}] (* expected result *)

(* We expect this to fail, the shape annotation is too restrictive. *)
type t = int [@@shape [imm 0]]
[%%expect {|
Line 1, characters 0-30:
1 | type t = int [@@shape [imm 0]]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this type declaration, the actual head shape does not match the
       expected type shape.
|}] (* expected result *)


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
|}] (* expected result *)

(* We expect this to fail: the implementation does not match the signature *)
module Invalid : ImmOrFun = struct
  type t = int list
end
[%%expect {|
Lines 1-3, characters 28-3:
1 | ............................struct
2 |   type t = int list
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int list end
       is not included in
         ImmOrFun
       Type declarations do not match:
         type t = int list
       is not included in
         type t
       The shape of the type provided,
         {imm = [0]; blocks = [0 size [2]];},
       is not included in the expected shape,
         {imm = Any; blocks = [closure; infix];}.
|}] (* expected result *)

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
  type t = T.t [@@shape [string]]
end
[%%expect {|
Line 2, characters 2-33:
2 |   type t = T.t [@@shape [string]]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this type declaration, the actual head shape does not match the
       expected type shape.
|}] (* expected result *)

(* This functor application should be rejected. *)
module InvalidApplication = Valid(struct type t = string end)
[%%expect {|
Line 1, characters 28-61:
1 | module InvalidApplication = Valid(struct type t = string end)
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match: sig type t = string end is not included in
       ImmOrFun
     Type declarations do not match:
       type t = string
     is not included in
       type t
     The shape of the type provided,
       {imm = []; blocks = [string];},
     is not included in the expected shape,
       {imm = Any; blocks = [closure; infix];}.
|}] (* expected result *)

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
|}] (* Yes, we get back a narrower type,
       otherwise this would not be accepted. *)
