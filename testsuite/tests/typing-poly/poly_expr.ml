(* TEST
   * expect
*)

type nat = < poly : 'a. ('a -> 'a) -> ('a -> 'a) >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
[%%expect{|
type nat = < poly : 'a. ('a -> 'a) -> 'a -> 'a >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
|}]

let zero : nat = [%poly fun f x -> x]
let one : nat = [%poly fun f x -> f x]
let suc : nat -> nat = fun n -> [%poly fun f x -> n#poly f (f x)]
[%%expect{|
val zero : nat = <obj>
val one : nat = <obj>
val suc : nat -> nat = <fun>
|}]

let rec of_int' n f x =
  if n <= 0 then x else of_int' (n-1) f (f x)
let of_int n : nat = [%poly of_int' n]
let to_int (n : nat) = n#poly succ 0
[%%expect{|
val of_int' : int -> ('a -> 'a) -> 'a -> 'a = <fun>
val of_int : int -> nat = <fun>
val to_int : nat -> int = <fun>
|}]

let add (m : nat) (n : nat) : nat =
  [%poly fun f x -> m#poly f (n#poly f x)]

let mul : nat -> nat -> nat =
  fun m n -> [%poly fun f -> m#poly (n#poly f)]

let pow : nat -> nat -> nat =
  fun m n -> [%poly n#poly m#poly]
[%%expect{|
val add : nat -> nat -> nat = <fun>
val mul : nat -> nat -> nat = <fun>
val pow : nat -> nat -> nat = <fun>
|}]

let test1 = to_int (pow (of_int 2) (of_int 3))
[%%expect{|
val test1 : int = 8
|}]
