(* TEST
  expect;
*)

(* PR#12959  *)

module rec A: sig
  val x: 'a B.t -> unit
end = struct
  let x _ = ()
end
and B: sig
  type +'a t
end = struct
  type t
end
  [%%expect {|
Line 1:
Error: Modules do not match:
         sig type t end
       is not included in
         sig type +'a t end
       Type declarations do not match: type t is not included in type +'a t
       They have different arities.
|}]

module rec A: sig
  val x: 'a F(B).t -> unit
end = struct
  let x _ = ()
end
and B: sig
  type 'a t
  type x
end = struct
  type 'a t
  type x
end
and F: functor(X:sig type x end) -> sig type 'a t = 'a * X.x end =
  functor(X:sig type y end) -> struct type t = int end
  [%%expect {|
Line 1:
Error: Modules do not match:
         (X : $S1) -> ...
       is not included in
         (X : $T1) -> ...
       Module types do not match:
         $S1 = sig type y end
       does not include
         $T1 = sig type x end
       The type "y" is required but not provided
|}]

module type S = sig type 'a t end
module rec A: sig val x: 'a B.t -> unit end = struct
        let x _ = ()
end
and B : S = struct type t end;;
[%%expect {|
module type S = sig type 'a t end
Line 1:
Error: Modules do not match: sig type t end is not included in S
       Type declarations do not match: type t is not included in type 'a t
       They have different arities.
|}]

module rec A: sig val x: 'a B.M.t -> unit end = struct
  let x _ = ()
end
and B: sig module type S=sig type 'a t end module M:S end = struct
        module type S = sig type t end
        module M: S = struct type t end
       end
[%%expect {|
Line 1:
Error: Modules do not match:
         sig module type S = sig type t end module M : S end
       is not included in
         sig module type S = sig type 'a t end module M : S end
       Module type declarations do not match:
         module type S = sig type t end
       does not match
         module type S = sig type 'a t end
       At position "module type S = <here>"
       Module types do not match:
         sig type t end
       is not equal to
         sig type 'a t end
       At position "module type S = <here>"
       Type declarations do not match: type t is not included in type 'a t
       They have different arities.
|}]
