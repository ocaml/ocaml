(* TEST
   * expect
*)

type 'a t = [< `Foo | `Bar] as 'a;;
type 'a s = [< `Foo | `Bar | `Baz > `Bar] as 'a;;

type 'a first = First : 'a second -> ('b t as 'a) first
and 'a second = Second : ('b s as 'a) second;;

type aux = Aux : 'a t second * ('a -> int) -> aux;;

let it : 'a. [< `Bar | `Foo > `Bar ] as 'a = `Bar;;

let g (Aux(Second, f)) = f it;;

[%%expect{|
type 'a t = 'a constraint 'a = [< `Bar | `Foo ]
type 'a s = 'a constraint 'a = [< `Bar | `Baz | `Foo > `Bar ]
type 'a first = First : 'a t second -> ([< `Bar | `Foo ] as 'a) t first
and 'a second = Second : [< `Bar | `Baz | `Foo > `Bar ] s second
type aux = Aux : ([< `Bar | `Foo ] as 'a) t second * ('a -> int) -> aux
val it : [< `Bar | `Foo > `Bar ] = `Bar
Line 11, characters 27-29:
11 | let g (Aux(Second, f)) = f it;;
                                ^^
Error: This expression has type [< `Bar | `Foo > `Bar ]
       but an expression was expected of type [< `Bar | `Foo ]
       The second variant type is bound to $Aux_'a,
       it may not allow the tag(s) `Bar
|}];;
