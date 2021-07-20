(* TEST
  * expect
 *)

module M :
     sig type t = private [< `A | `B of string] end
= struct type t = [`A|`B of string] end;;
[%%expect{|
module M : sig type t = private [< `A | `B of string ] end
|}]

module M = struct type header_item_tag =
    [ `CO | `HD | `Other of string | `PG | `RG | `SQ ]
end;;
[%%expect{|
module M :
  sig
    type header_item_tag = [ `CO | `HD | `Other of string | `PG | `RG | `SQ ]
  end
|}]

module M' : sig type header_item_tag =
    private [< `CO | `HD | `Other of string | `PG | `RG | `SQ ]
end = M;;
[%%expect{|
module M' :
  sig
    type header_item_tag = private
        [< `CO | `HD | `Other of string | `PG | `RG | `SQ ]
  end
|}]
