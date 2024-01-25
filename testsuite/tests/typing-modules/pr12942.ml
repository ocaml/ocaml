(* TEST
 expect;
*)

(* This test ensures that the error messages for missing bindings
   are printed in a logical order, with "In module N:" mentioned
   in sensible places.
 *)

module M : sig
  module N : sig
    type t
    val x : t
    val y : t
  end
end = struct
  module N = struct
  end
end

[%%expect {|
Lines 7-10, characters 6-3:
 7 | ......struct
 8 |   module N = struct
 9 |   end
10 | end
Error: Signature mismatch:
       Modules do not match:
         sig module N : sig end end
       is not included in
         sig module N : sig type t val x : t val y : t end end
       In module "N":
       Modules do not match:
         sig end
       is not included in
         sig type t val x : t val y : t end
       In module "N":
       The type "t" is required but not provided
       The value "x" is required but not provided
       The value "y" is required but not provided
|}];;
