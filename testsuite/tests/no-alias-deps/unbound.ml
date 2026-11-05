(* TEST
   flags="-no-alias-deps";
   expect;
*)

(* Test the error message for an unbound static alias *)

module M: sig module Alias:sig end end = struct
  module Alias = Unknown
end
[%%expect {|
Lines 1-3, characters 41-3:
1 | .........................................struct
2 |   module Alias = Unknown
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig module Alias = Unknown end
       is not included in
         sig module Alias : sig end end
       In module "Alias":
       A static module alias refers to the unbound module "Unknown"
|}]
