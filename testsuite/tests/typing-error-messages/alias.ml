module M: sig
        val f: ([`A|`B|`C] as 'a) -> 'a -> 'a 
end = struct let f x y = x + y end;;

[%%expect{|
Line _, characters 6-34:
Error: Signature mismatch:
       Modules do not match:
         sig val f : !(int) -> ... -> !(int) end
       is not included in
         sig val f : !([ `A | `B | ... ]) -> ... -> !([ ... * 3 ]) end
       Values do not match:
         val f : !(int) -> ... -> !(int)
       is not included in
         val f : !([ `A | `B | ... ]) -> ... -> !([ ... * 3 ])
|}]
