(* TEST
   * expect
*)

type +'a n = private int
type nil = private Nil_type
type (_,_) elt =
  | Elt_fine: 'nat n -> ('l,'nat * 'l) elt
  | Elt: 'nat n -> ('l,'nat -> 'l) elt
type _ t = Nil : nil t | Cons : ('x, 'fx) elt * 'x t -> 'fx t;;

let undetected: ('a -> 'b -> nil) t -> 'a n -> 'b n -> unit = fun sh i j ->
  let Cons(Elt dim, _) = sh in ()
;;

[%%expect{|
type +'a n = private int
type nil = private Nil_type
type (_, _) elt =
    Elt_fine : 'nat n -> ('l, 'nat * 'l) elt
  | Elt : 'nat n -> ('l, 'nat -> 'l) elt
type _ t = Nil : nil t | Cons : ('x, 'fx) elt * 'x t -> 'fx t
Line 9, characters 11-18:
9 |   let Cons(Elt dim, _) = sh in ()
               ^^^^^^^
Error: This pattern matches values of type ($Cons_'x, 'a -> $Cons_'x) elt
       but a pattern was expected which matches values of type
         ($Cons_'x, 'a -> $'b -> nil) elt
       The type constructor $'b would escape its scope
|}, Principal{|
type +'a n = private int
type nil = private Nil_type
type (_, _) elt =
    Elt_fine : 'nat n -> ('l, 'nat * 'l) elt
  | Elt : 'nat n -> ('l, 'nat -> 'l) elt
type _ t = Nil : nil t | Cons : ('x, 'fx) elt * 'x t -> 'fx t
Line 9, characters 6-22:
9 |   let Cons(Elt dim, _) = sh in ()
          ^^^^^^^^^^^^^^^^
Error: This pattern matches values of type ('a -> $0 -> nil) t
       The type constructor $0 would escape its scope
|}];;
