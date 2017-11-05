let o = object method f=0; method g =1; method h = 2 end
let f o = o#f + o#g + o#h + o#i
;; f o;;
[%%expect {|
val o : < f : int; g : int; h : int > = <obj>
val f : < f : int; g : int; h : int; i : int; .. > -> int = <fun>
Line _, characters 5-6:
Error: This expression has type < ... * 3!() >
       but an expression was expected of type < ... * 3; !(i : int); !(..) >
       The first object type has no method i
|}]


type c =
  < arma:int; virumque:int; cano:int; troiae:float; qui:float;
    primus:char; ab:int; oris:float; italiam:float; fato:unit; profugus:int list >
let o : c =  object
        method primus='x'
        method ab=0
        method oris=1
        method italiam=1.
        method fato = ()
        method profugus = [fun x -> x]
        method laviniaque = "u"
        method venit = ()
        method litora x = 1 + x
        method multum x = 1. +. x
        method et l = 1 :: l
        method terris = 1,0
        method jactatus _ _ = ()
        method et' = [2.]
        method alto = [[]]
      end;;
[%%expect {|
type c =
    < ab : int; arma : int; cano : int; fato : unit; italiam : float;
      oris : float; primus : char; profugus : int list; qui : float;
      troiae : float; virumque : int >
Line _, characters 13-454:
Error: This expression has type
         < ...; !(alto : ... list); ... * 10; oris : !(int); ... * 7 >
       but an expression was expected of type
         c = < ... * 11; oris : !(float); ... * 7 >
       The second object type has no method alto
|}]

let o = object method f=0; method g =1; method h = 2 end
let f o = o#f + o#g + o#h + o#i
;; f o;;
[%%expect {|
val o : < f : int; g : int; h : int > = <obj>
val f : < f : int; g : int; h : int; i : int; .. > -> int = <fun>
Line _, characters 5-6:
Error: This expression has type < ... * 3!() >
       but an expression was expected of type < ... * 3; !(i : int); !(..) >
       The first object type has no method i
|}]


let (=) (f:<a:int;b:int;..>) (g:<a:float;c:float;..>) = f = g;;
[%%expect {|
Line _, characters 60-61:
Error: This expression has type < a : !(float); ...; .. >
       but an expression was expected of type < a : !(int); b : int; ...; .. >
       Types for method a are incompatible
|}]
