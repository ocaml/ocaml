type t = [`A]
let f x = match x with
|` B -> 0;;
f `A;;
[%%expect {|
type t = [ `A ]
val f : [< `B ] -> int = <fun>
Line _, characters 2-4:
Error: This expression has type [!(> )!(`A)!() ]
       but an expression was expected of type [!(< )!(`B)!() ]
       The second variant type does not allow tag(s) `A
|}]

module M:sig type t = [`B of int | `A ] end = struct
        type t = [ `C | `B of float ] 
end;;
[%%expect {|
Line _, characters 46-95:
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `B of !(float) | ... ] end
       is not included in
         sig type t = [ !(`A) | `B of !(int) | ... ] end
       Type declarations do not match:
         type t = [ `B of !(float) | ... ]
       is not included in
         type t = [ !(`A) | `B of !(int) | ... ]
|}]


type t=[`A | `B of float]
type 'a u=[<`A|`B of int] as 'a
let x: t = (`B:'a u:>t);;
[%%expect {|
type t = [ `A | `B of float ]
type 'a u = 'a constraint 'a = [< `A | `B of int ]
Line _, characters 11-23:
Error: Type ([< `A | `B of int ] as 'a) u = 'a is not a subtype of
         t = [ `A | `B of float ]
       Type !(int) is not a subtype of !(float)
|}]


type t = [`A|`B]
let f = function `C -> 0;;
let g (f:t -> float) x = f x;;
g f `A;;
[%%expect {|
type t = [ `A | `B ]
val f : [< `C ] -> int = <fun>
val g : (t -> float) -> t -> float = <fun>
Line _, characters 2-3:
Error: This expression has type (!([< `C ]) as 'a) -> !(int)
       but an expression was expected of type !(t) -> !(float)
       Type [< ...!() ] as 'a is not compatible with type t = [ !(`A) | !(`B) | ...!() ]
       These two variant types have no intersection
|}]
