(* TEST
   * expect
*)

type nat = < app : 'a. ('a -> 'a) -> ('a -> 'a) >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
[%%expect{|
type nat = < app : 'a. ('a -> 'a) -> 'a -> 'a >
type 'a nat' = ('a -> 'a) -> 'a -> 'a
|}]

let zero : nat = [%poly (fun f x -> x : _ nat')]
let one : nat = [%poly (fun f x -> f x : _ nat')]
let suc : nat -> nat = fun n -> [%poly fun f x -> n#app f (f x)]
[%%expect{|
val zero : nat = <obj>
val one : nat = <obj>
val suc : nat -> nat = <fun>
|}]

let rec of_int' n f x =
  if n <= 0 then x else of_int' (n-1) f (f x)
let of_int n : nat = [%poly of_int' n]
let to_int (n : nat) = n#app succ 0
[%%expect{|
val of_int' : int -> ('a -> 'a) -> 'a -> 'a = <fun>
val of_int : int -> nat = <fun>
val to_int : nat -> int = <fun>
|}]

let add (m : nat) (n : nat) : nat =
  [%poly fun f x -> m#app f (n#app f x)]

let mul : nat -> nat -> nat =
  fun m n -> [%poly fun f -> m#app (n#app f)]

let pow : nat -> nat -> nat =
  fun m n -> [%poly n#app m#app]
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
let auto (x : <app : 'a. 'a -> 'a>) = x#app x
let auto' (x : <app : 'a. 'a -> 'a>) = x#app x#app
let a5 = id auto
let a6 () = id auto'
let a6' = id [%poly auto']
let a7 = choose id auto
let poly (id : <app : 'a. 'a -> 'a>) = id#app 1, id#app true
let a10 = poly [%poly id]
let a11 = poly [%poly fun x -> x]
let a12 = id poly [%poly fun x -> x]
[%%expect{|
val id : 'a -> 'a = <fun>
val ids : < app : 'a. 'a -> 'a > list = [<obj>]
val choose : 'a -> 'a -> 'a = <fun>
val a2 : unit -> ('a -> 'a) -> 'a -> 'a = <fun>
val a2' : < app : 'a. 'a -> 'a > -> < app : 'b. 'b -> 'b > = <fun>
val a3 : < app : 'a. 'a -> 'a > list = []
val auto : < app : 'a. 'a -> 'a > -> < app : 'a. 'a -> 'a > = <fun>
val auto' : < app : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val a5 : < app : 'a. 'a -> 'a > -> < app : 'a. 'a -> 'a > = <fun>
val a6 : unit -> < app : 'a. 'a -> 'a > -> 'b -> 'b = <fun>
val a6' : < app : 'b. < app : 'a. 'a -> 'a > -> 'b -> 'b > = <obj>
val a7 : < app : 'a. 'a -> 'a > -> < app : 'a. 'a -> 'a > = <fun>
val poly : < app : 'a. 'a -> 'a > -> int * bool = <fun>
val a10 : int * bool = (1, true)
val a11 : int * bool = (1, true)
val a12 : int * bool = (1, true)
|}]
