(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints on integer equality tests *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"

let int_equal (x : int) y = x = y

let int_not_equal (x : int) y = x <> y

(* Ordering comparisons are always on integers: no hint *)
let int_less_than (x : int) y = x < y

(* Physical comparisons: no hint *)
let physical_equal (x : string) y = x == y

let physical_not_equal (x : string) y = x != y
