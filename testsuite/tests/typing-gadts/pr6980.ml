type 'a t = [< `Foo | `Bar] as 'a;;
type 'a s = [< `Foo | `Bar | `Baz > `Bar] as 'a;;

type 'a first = First : 'a second -> ('b t as 'a) first
and 'a second = Second : ('b s as 'a) second;;

type aux = Aux : 'a t second * ('a -> int) -> aux;;

let it : 'a. [< `Bar | `Foo > `Bar ] as 'a = `Bar;;

let g (Aux(Second, f)) = f it;;
