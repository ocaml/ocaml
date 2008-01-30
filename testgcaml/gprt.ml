open Format
open Gprint
let _ = print std_formatter (1,[2;3])
let _ = print_newline ()
let _ = print std_formatter (Some (Some (Some None)))
let _ = print_newline ()
type t1 = Foo of int | Bar of t2
and t2 = { x : int; mutable y : t1 option }
let _ = print std_formatter 
    (Bar {x= 0; 
	  y= Some (Bar {x= 1; 
			y= Some (Foo 2)})})
let _ = print_newline ()
