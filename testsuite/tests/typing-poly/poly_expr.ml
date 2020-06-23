(* TEST
   * expect
*)

type nat = < poly : 'a. ('a -> 'a) -> ('a -> 'a) >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
[%%expect{|
type nat = < poly : 'a. ('a -> 'a) -> 'a -> 'a >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
|}]

let zero : nat = [%poly (fun f x -> x : _ nat')]
let one : nat = [%poly (fun f x -> f x : _ nat')]
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


let id x = x
let ids = [[%poly id]]
let choose x y = if true then x else y
let a2 () = choose id
let a2' = choose [%poly id]
let a3 = choose [] ids
let auto (x : <poly : 'a. 'a -> 'a>) = x#poly x
let auto' (x : <poly : 'a. 'a -> 'a>) = x#poly x#poly
let a5 = id auto
let a6 () = id auto'
let a6' = id [%poly auto']
let a7 = choose id auto
let poly (id : <poly: 'a. 'a -> 'a>) = id#poly 1, id#poly true
let a10 = poly [%poly id]
let a11 = poly [%poly fun x -> x]
let a12 = id poly [%poly fun x -> x]
[%%expect{|
val id : 'a -> 'a = <fun>
val ids : < poly : 'a. 'a -> 'a > list = [<obj>]
val choose : 'a -> 'a -> 'a = <fun>
val a2 : unit -> ('a -> 'a) -> 'a -> 'a = <fun>
val a2' : < poly : 'a. 'a -> 'a > -> < poly : 'b. 'b -> 'b > = <fun>
val a3 : < poly : 'a. 'a -> 'a > list = []
val auto : < poly : 'a. 'a -> 'a > -> < poly : 'a. 'a -> 'a > = <fun>
val auto' : < poly : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val a5 : < poly : 'a. 'a -> 'a > -> < poly : 'a. 'a -> 'a > = <fun>
val a6 : unit -> < poly : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val a6' : < poly : 'b. < poly : 'a. 'a -> 'a > -> 'b -> 'b > = <obj>
val a7 : < poly : 'a. 'a -> 'a > -> < poly : 'a. 'a -> 'a > = <fun>
val poly : < poly : 'a. 'a -> 'a > -> int * bool = <fun>
val a10 : int * bool = (1, true)
val a11 : int * bool = (1, true)
val a12 : int * bool = (1, true)
|}]
