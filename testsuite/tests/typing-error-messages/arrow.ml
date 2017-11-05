
module F: sig
        val f:int -> int -> int -> int -> int -> int -> int
end = struct
        let f _ _ _ _ x y = x +. y
end;;
[%%expect {|
Line _, characters 6-51:
Error: Signature mismatch:
       Modules do not match:
         sig val f : ... * 4 -> !(float) -> ... -> !(float) end
       is not included in
         sig val f : ... * 4 -> !(int) -> ... -> !(int) end
       Values do not match:
         val f : ... * 4 -> !(float) -> ... -> !(float)
       is not included in
         val f : ... * 4 -> !(int) -> ... -> !(int)
|}]

module G: sig
        val f: ?xyz:int -> int -> int -> int -> int -> int -> int
end = struct
        let f ?xzy:_ _ _ _ x y = x + y
end;;
[%%expect {|
Line _, characters 6-55:
Error: Signature mismatch:
       Modules do not match:
         sig val f : !(?xzy):'a -> ... * 5 -> int end
       is not included in
         sig val f : !(?xyz):int -> ... * 5 -> int end
       Values do not match:
         val f : !(?xzy):'a -> ... * 5 -> int
       is not included in
         val f : !(?xyz):int -> ... * 5 -> int
|}]

type x
type ui type superum type saevae type memorem type luonis type ob type iram
type multa type quoque type et type bello type passus type dum type conderet
type urbem

module M: sig
  val f: ui -> superum -> saevae -> memorem -> luonis -> ob -> iram
  -> multa -> quoque -> et -> bello -> passus -> dum -> conderet
  -> urbem -> unit
end = struct
  let f (_:ui) (_:x) (_:saevae) (_:x) (_:luonis) (_:x) (_:iram)
  (_:x) (x:quoque) (_:x) (_:bello) (_:x) (_:dum)
  (_:x) (_:urbem) = ()
end;;
[%%expect {|
type x
type ui
type superum
type saevae
type memorem
type luonis
type ob
type iram
type multa
type quoque
type et
type bello
type passus
type dum
type conderet
type urbem
Line _, characters 6-152:
Error: Signature mismatch:
       Modules do not match:
         sig val f : ... -> !(x) -> ... -> !(x) -> ... * 11 -> unit end
       is not included in
         sig val f : ... -> !(superum) -> ... -> !(memorem) -> ... * 11 -> unit end
       Values do not match:
         val f : ... -> !(x) -> ... -> !(x) -> ... * 11 -> unit
       is not included in
         val f : ... -> !(superum) -> ... -> !(memorem) -> ... * 11 -> unit
|}]

module M:
        sig
                val f: lbl:unit -> ?opt:unit -> unit -> unit
        end=
        struct
                let f ~lbll ?optt () = ()
        end
;;
[%%expect {|
Line _, characters 8-68:
Error: Signature mismatch:
       Modules do not match:
         sig val f : !(lbll):'a -> !(?optt):'b -> ... -> unit end
       is not included in
         sig val f : !(lbl):unit -> !(?opt):unit -> ... -> unit end
       Values do not match:
         val f : !(lbll):'a -> !(?optt):'b -> ... -> unit
       is not included in
         val f : !(lbl):unit -> !(?opt):unit -> ... -> unit
|}]

module M:
        sig
                val f: unit -> unit -> unit -> unit -> unit -> unit
        end=
        struct
                let f x y z w t= x + y + z + w + t
        end
;;
[%%expect {|
Line _, characters 8-77:
Error: Signature mismatch:
       Modules do not match:
         sig val f : !(int) -> ... * 4 -> !(int) end
       is not included in
         sig val f : !(unit) -> ... * 4 -> !(unit) end
       Values do not match:
         val f : !(int) -> ... * 4 -> !(int)
       is not included in
         val f : !(unit) -> ... * 4 -> !(unit)
|}]

let f: int -> int -> int -> int -> int -> int = 0

module M: sig
  val odd:  int -> unit -> int -> unit -> int -> unit
  val even: unit -> int -> unit -> int -> unit -> int
  val t_3: unit -> unit -> int -> unit -> unit -> int -> unit -> unit -> int
  val t_2: unit -> int -> unit -> unit -> int -> unit -> unit -> int -> unit
  val t_1: int -> unit -> unit -> int -> unit -> unit -> int -> unit -> unit
end = struct

  let odd a b c d e = a + b + c + d + e

  let t_3 a b c d e f g h = a + b + c + d + e + f + g + h

  let t_1 a b c d e f g h = a + b + c + d + e + f + g + h

end;;
[%%expect {|
Line _, characters 48-49:
Error: This expression has type !(int) but an expression was expected of type
         !(int -> ... * 4 -> int)
|}]

module F: sig
        val f:int -> int -> int -> int -> int -> int -> int
end = struct
        let f _ _ _ _ x y = x +. y
end;;
[%%expect {|
Line _, characters 6-51:
Error: Signature mismatch:
       Modules do not match:
         sig val f : ... * 4 -> !(float) -> ... -> !(float) end
       is not included in
         sig val f : ... * 4 -> !(int) -> ... -> !(int) end
       Values do not match:
         val f : ... * 4 -> !(float) -> ... -> !(float)
       is not included in
         val f : ... * 4 -> !(int) -> ... -> !(int)
|}]

let f a b c d e = a+ b + c + d +e;;
let g f = f () 1 () 2 ();;
g f;;
[%%expect {|
val f : int -> int -> int -> int -> int -> int = <fun>
val g : (unit -> int -> unit -> int -> unit -> 'a) -> 'a = <fun>
Line _, characters 2-3:
Error: This expression has type !(int) -> ... -> !(int) -> ... * 2 -> int
       but an expression was expected of type
         !(unit) -> ... -> !(unit) -> ... * 2 -> 'a
       Type !(int) is not compatible with type !(unit)
|}]

type 'a t = A
module M: sig val enum: ?docv:string -> (string * 'a list) -> 'a t end = struct
  let enum ?(dovc="None") _ = A
end;;
[%%expect {|
type 'a t = A
Line _, characters 73-115:
Error: Signature mismatch:
       Modules do not match:
         sig val enum : !(?dovc):string -> ... -> ... t end
       is not included in
         sig val enum : !(?docv):string -> ... -> ... t end
       Values do not match:
         val enum : !(?dovc):string -> ... -> ... t
       is not included in
         val enum : !(?docv):string -> ... -> ... t
|}
]
