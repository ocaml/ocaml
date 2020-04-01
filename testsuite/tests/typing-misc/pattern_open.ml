(* TEST
   * expect
*)

let pp fmt = Format.printf (fmt^^"@.")

type 'a box = B of 'a
(* Basic tests *)
module M = struct
  type c = C
  type t = {x : c box }
end

module N = struct
  type d = D
  let d = D
  type t = {x: d box}
end
open N;;
[%%expect {|
val pp : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a = <fun>
type 'a box = B of 'a
module M : sig type c = C type t = { x : c box; } end
module N : sig type d = D val d : d type t = { x : d box; } end
|}]

let f M.{ x=B C } y  = M.C,y;;
let g M.(x) M.(w) = x * w
;;
let g = function
  | M.[] -> []
  | M.[C] -> M.[C]
  | _ -> []
;;
let h = function
  | M.[||] -> None
  | M.[| C |] -> Some M.C
  | _ -> None
;;
let f2 = function
  | M.( B (B C) ) -> M.C
;;
(* () constructor *)
let M.() = ()
;;
[%%expect {|
val f : M.t -> 'a -> M.c * 'a = <fun>
val g : int -> int -> int = <fun>
val g : M.c list -> M.c list = <fun>
val h : M.c array -> M.c option = <fun>
val f2 : M.c box box -> M.c = <fun>
|}]

(* Pattern open separation*)
module L = struct
  type _ c = C : unit c
  type t = { t : unit c }
  type r = { r : unit c }
  let x ()= pp "Wrong value L.x"
end
;;
module K = struct
  type _ c = C : unit c
  type t = { t : unit c }
  type r = { r : unit c }
  let x ()= pp "Right value K.x"
end;;
[%%expect {|
module L :
  sig
    type _ c = C : unit c
    type t = { t : unit c; }
    type r = { r : unit c; }
    val x : unit -> unit
  end
module K :
  sig
    type _ c = C : unit c
    type t = { t : unit c; }
    type r = { r : unit c; }
    val x : unit -> unit
  end
|}]

let () =
  let test =
    let open K in
    function
    | L.{t}, ({r=C} : K.r)  -> x ()
  in
  test (L.{t=C}, K.{r=C});;
[%%expect {|
Right value K.x
|}]

module Exterior = struct
  module Gadt = struct
    module Boolean = struct
      type t = { b : bool }
      type wrong = false | true
      let print () = pp "Wrong function: Exterior.Gadt.Boolean.print"
    end

    type _ t =
      | Bool : Boolean.t -> bool t
      | Int : int -> int t
      | Eq : 'a t * 'a t -> bool t

    let print () = pp "Wrong function: Exterior.Gadt.print"
  end
  let print () = pp "Wrong function: Exterior.print"
end;;
[%%expect {|
module Exterior :
  sig
    module Gadt :
      sig
        module Boolean :
          sig
            type t = { b : bool; }
            type wrong = false | true
            val print : unit -> unit
          end
        type _ t =
            Bool : Boolean.t -> bool t
          | Int : int -> int t
          | Eq : 'a t * 'a t -> bool t
        val print : unit -> unit
      end
    val print : unit -> unit
  end
|}]
let rec eval: type t. t Exterior.Gadt.t -> t = function
  | Exterior.( Gadt.( Eq (a,b) ) ) -> (eval a) = (eval b)
  | Exterior.( Gadt.( Bool Boolean.{b} ) ) -> b
  | Exterior.Gadt.( Int n ) -> n
let () =
  let print () = pp "Right function print" in
  let choose (type a):a Exterior.Gadt.t * a Exterior.Gadt.t -> a -> a =
    fun (a,b) c ->
      match a, b, c with
      | Exterior.( Gadt.( Bool Boolean.{b} ), Gadt.Bool _ , _ ) -> print(); true
      | Exterior.(Gadt.Bool Gadt.Boolean.{b}), _ , true -> print(); true
      | Exterior.(Gadt.Bool Gadt.Boolean.{b}), _ , false -> print(); b
      | Exterior.Gadt.( Int n, Int k, 0 ) -> print(); 0
      | Exterior.( Gadt.(Int n, Gadt.Int k, l) ) -> print(); k+n+l
      | Exterior.Gadt.( Eq (a,b) ), _,  true -> print(); true
      | Exterior.(Gadt.( Eq (a,b), _ ,  false )) -> print(); eval a = eval b in
  let _ =
    choose Exterior.Gadt.(Bool Boolean.{b=true}, Bool Boolean.{b=false}) false
  in
  print ()
;;
[%%expect {|
val eval : 't Exterior.Gadt.t -> 't = <fun>
Right function print
Right function print
|}
]
(* existential type *)
module Existential = struct
  type printable = E : 'a * ('a -> unit) -> printable
end

let rec print: Existential.printable -> unit  = function
  | Existential.( E(x, print) ) -> print x
;;
[%%expect {|
module Existential :
  sig type printable = E : 'a * ('a -> unit) -> printable end
val print : Existential.printable -> unit = <fun>
|}]

(* Test that constructors and variables introduced in scope inside
   M.(..) are not propagated outside of M.(..) *)
module S = struct
  type 'a t = Sep : unit t
  type ex = Ex: 'a * 'a -> ex
  let s = Sep
end
;;
[%%expect {|
module S :
  sig
    type 'a t = Sep : unit t
    type ex = Ex : 'a * 'a -> ex
    val s : unit t
  end
|}]
let test_separation = function
  | S.(Sep), (S.(Sep,Sep), Sep) -> ();;
[%%expect {|
Line 2, characters 27-30:
2 |   | S.(Sep), (S.(Sep,Sep), Sep) -> ();;
                               ^^^
Error: Unbound constructor Sep
|}]
let test_separation_2 = function
  | S.(Ex(a,b)), Ex(c,d) -> ();;
[%%expect {|
Line 2, characters 17-19:
2 |   | S.(Ex(a,b)), Ex(c,d) -> ();;
                     ^^
Error: Unbound constructor Ex
|}]
let test_separation_3 = function
  | S.(Sep) -> s;;
[%%expect {|
Line 2, characters 15-16:
2 |   | S.(Sep) -> s;;
                   ^
Error: Unbound value s
|}]

(* Testing interaction of local open in pattern and backtracking *)
module PR6437 = struct
  module Ctx = struct
    type ('a, 'b) t =
      | Nil : (unit, unit) t
      | Cons : ('a, 'b) t -> ('a * unit, 'b * unit) t
  end

  module Var = struct
    type 'a t =
      | O : ('a * unit) t
      | S : 'a t -> ('a * unit) t
  end
end

let rec f : type g1 g2. (g1, g2) PR6437.Ctx.t * g1 PR6437.Var.t
  -> g2 PR6437.Var.t = function
  | PR6437.( Ctx.(Cons g), Var.(O) ) -> PR6437.Var.O
  | PR6437.( Ctx.(Cons g), Var.(S n) ) -> PR6437.Var.S (f (g, n))
  | _ -> . ;;
[%%expect {|
module PR6437 :
  sig
    module Ctx :
      sig
        type ('a, 'b) t =
            Nil : (unit, unit) t
          | Cons : ('a, 'b) t -> ('a * unit, 'b * unit) t
      end
    module Var :
      sig type 'a t = O : ('a * unit) t | S : 'a t -> ('a * unit) t end
  end
val f : ('g1, 'g2) PR6437.Ctx.t * 'g1 PR6437.Var.t -> 'g2 PR6437.Var.t =
  <fun>
|}]
