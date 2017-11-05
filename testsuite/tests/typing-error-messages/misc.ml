module M: sig type t [@@immediate] end = struct type t[@@imediate] end ;;
[%%expect {|
Line _, characters 41-70:
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t!( [@@immediate]) end
       Type declarations do not match:
         type t
       is not included in
         type t!( [@@immediate])
       the first is not an immediate type.
|}]

module M: sig type -'a t end = struct type +'a t end;;
[%%expect {|
Line _, characters 31-52:
Error: Signature mismatch:
       Modules do not match:
         sig type !(+)'a t end
       is not included in
         sig type !(-)'a t end
       Type declarations do not match:
         type !(+)'a t
       is not included in
         type !(-)'a t
       Their variances do not agree.
|}]
