(* TEST
 flags = "-I ${ocamlsrcdir}/utils";
 expect;
*)

#rectypes;;

let x =
  fun (f : (?opt:int -> 'a) as 'a) -> f 3;;

[%%expect{|
Line 2, characters 40-41:
2 |   fun (f : (?opt:int -> 'a) as 'a) -> f 3;;
                                            ^
Error: The function applied to this argument has type
         ?opt:int -> ?opt:int -> (?opt:int -> 'a as 'a)
This argument cannot be applied without label
|}]


let x =
  fun (f : (x:string -> y:int -> 'a) as 'a) -> f 3;;

[%%expect{|
Line 2, characters 49-50:
2 |   fun (f : (x:string -> y:int -> 'a) as 'a) -> f 3;;
                                                     ^
Error: The function applied to this argument has type
         x:string -> y:int -> x:string -> (y:int -> x:string -> 'a as 'a)
This argument cannot be applied without label
|}]

let rec f ~x ~y = Format.printf "@[x=%s y=%s@]@." x y; f

let u () =
  let f = f ~x:"hello" ~x:"second hello" ~x:"last" in
  Format.printf "Nothing yet@.";
  let f = f ~y:"world" in
  Format.printf "!@.";
  let f = f ~y:"universe" in
  Format.printf "!@.";
  ignore f

[%%expect{|
val f : x:string -> y:string -> 'a as 'a = <fun>
val u : unit -> unit = <fun>
|}, Principal{|
val f : x:string -> y:string -> (x:string -> y:string -> 'a as 'a) = <fun>
val u : unit -> unit = <fun>
|}]

let f g = g ?x:(g ?x:(Some g)) 0
[%%expect{|
Line 1, characters 15-30:
1 | let f g = g ?x:(g ?x:(Some g)) 0
                   ^^^^^^^^^^^^^^^
Error: This expression has type "'a -> 'b"
       but an expression was expected of type
         "(?x:'c -> 'a -> 'b as 'c) option"
Hint: This function application is partial, maybe some arguments are missing.
|}]

let f g = g ?x:(Some (g ?x:(Some g))) ?x:(Some g)
[%%expect{|
val f : (?x:'a -> 'a as 'a) -> 'a = <fun>
|}]

let rec f ?x = f
let () = Clflags.classic := true;;
[%%expect {|
Line 1, characters 11-12:
1 | let rec f ?x = f
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val f : ?x:'b -> 'a as 'a = <fun>
|}, Principal{|
Line 1, characters 11-12:
1 | let rec f ?x = f
               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val f : ?x:'a -> (?x:'a -> 'b as 'b) = <fun>
|}]

let () = f 3
[%%expect{|
Line 1, characters 11-12:
1 | let () = f 3
               ^
Error: The function applied to this argument has type
         ?x:'a -> ?x:'a -> (?x:'a -> 'b as 'b)
This argument cannot be applied without label
|}, Principal{|
Line 1, characters 11-12:
1 | let () = f 3
               ^
Error: The function applied to this argument has type
         ?x:'a -> ?x:'a -> ?x:'a -> (?x:'a -> 'b as 'b)
This argument cannot be applied without label
|}]
