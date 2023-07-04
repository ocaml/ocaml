(* TEST
 expect;
*)

type z
type 'a s
type _ nat =
  | Nz : z -> z nat
  | Nss : 'd nat -> 'd s s nat
  | Ns : 'a nat -> 'a s nat
;;

[%%expect{|
type z
type 'a s
Lines 3-6, characters 0-27:
3 | type _ nat =
4 |   | Nz : z -> z nat
5 |   | Nss : 'd nat -> 'd s s nat
6 |   | Ns : 'a nat -> 'a s nat
Error: In the GADT constructor
         "Nss : 'd nat -> 'd s s nat"
       the type variable "'d" cannot be deduced from the type parameters.
|}];;

type z
type 'a s
type _ nat = ..
type _ nat += Nz : z -> z nat
type _ nat += Nss : 'd nat -> 'd s s nat
;;

[%%expect{|
type z
type 'a s
type _ nat = ..
type _ nat += Nz : z -> z nat
Line 5, characters 0-40:
5 | type _ nat += Nss : 'd nat -> 'd s s nat
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the extension constructor
         type _ nat += Nss : 'd nat -> 'd s s nat
       the type variable "'d" cannot be deduced from the type parameters.
|}];;

type 'any any = int
type _ t = ..
type 'b t += A: 'b -> 'b any t

[%%expect{|
type 'any any = int
type _ t = ..
Line 3, characters 0-30:
3 | type 'b t += A: 'b -> 'b any t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the extension constructor
         type 'b t += A : 'b -> 'b any t
       the type variable "'b" cannot be deduced from the type parameters.
|}]
