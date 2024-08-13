(* TEST
   expect;
*)

external identity : 'a -> 'a = "%identity"

[%%expect {|
external identity : 'a -> 'a = "%identity"
|}]

(* Types are not unified in signatures. *)
module type Int_backed = sig
  type t
  external to_int : t -> int = identity
end

[%%expect {|
module type Int_backed =
  sig type t external to_int : t -> int = "%identity" end
|}]

(* Types are unified in structures; this should compile. *)
module Int_backed : Int_backed = struct
  type t = int
  external to_int = identity
end

[%%expect {|
module Int_backed : Int_backed
|}]

(* This should be rejected by the inclusion check. *)
module String_backed : Int_backed = struct
  type t = string
  external to_int = identity
end

[%%expect {|
Lines 1-4, characters 36-3:
1 | ....................................struct
2 |   type t = string
3 |   external to_int = identity
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string external to_int : 'a -> 'a = "%identity" end
       is not included in
         Int_backed
       Values do not match:
         external to_int : 'a -> 'a = "%identity"
       is not included in
         external to_int : t -> int = "%identity"
       The type "t -> t" is not compatible with the type "t -> int"
       Type "t" = "string" is not compatible with type "int"
|}]
