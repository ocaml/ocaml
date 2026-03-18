(* TEST
 expect;
*)

let _ =
  let type t = A in
  A
[%%expect{|
Line 3, characters 2-3:
3 |   A
      ^
Error: The constructor "A" has type "t" but an expression was expected of type "'a"
       The type constructor "t" would escape its scope
|}];;

let _ =
  let type t = .. in
  let type t += A in
  A
  [%%expect{|
Line 4, characters 2-3:
4 |   A
      ^
Error: The constructor "A" has type "t" but an expression was expected of type "'a"
       The type constructor "t" would escape its scope
|}];;

type u = ..

let _ =
  let type u += A in
  A
  [%%expect{|
type u = ..
- : u = <extension>
|}];;

let _ =
  let class c = object method f = 12 end in
  new c
  [%%expect{|
- : < f : int > = <obj>
|}];;

let _ =
  let external f : 'a -> 'a = "%identity" in
  f
[%%expect{|
- : 'a -> 'a = <fun>
|}];;

let _ =
  let type t = A of int | B in
  let _ = [A 42; B] in
  let type t = .. in
  let type t += A of string in
  let _ = A "hello" in
  let class c = object method f = 42 end in
  let class type ct = object method f : int end in
  let class d : ct = object (self) inherit c initializer print_int (self # f) end in
  let external f : 'a -> 'a = "%identity" in
  let [@@@warning "-unused-var"] in
  let v = (42, 12) in
  assert (f v == v);
  "OK"
[%%expect{|
- : string = "OK"
|}]


(* PR#14554, a regression reported by Antonio Monteiro.
   (The regressions or fixes are after 5.4, which is the last release
   without the generic [Pexp_struct_item] typing rules of #13839).

   In each example below, we expect the inferred type
   {[
     val dog : < bark : 'this -> unit > t as 'this
   ]}
   where the ['this] variable has been generalized,
   it is not a weak variable like ['_this].
*)

type 'a t
[%%expect{|
type 'a t
|}]

(* This was correct in OCaml 5.4,
   and was temporarily broken by #13839. *)
let dog : 'this =
  let module Dog = struct
    external make
      : bark:('self -> unit)
      -> < bark : ('self -> unit) > t = "%identity"
  end
  in
  Dog.make ~bark:(fun (o : 'this) -> ())
[%%expect{|
val dog : < bark : 'a -> unit > t as 'a = <abstr>
|}]

(* This variant from Samuel Vivien would also
   suffer from the same regression. *)
let dog : 'this =
  let
    external make
      : bark:('self -> unit)
      -> < bark : ('self -> unit) > t = "%identity"
  in
  make ~bark:(fun (o : 'this) -> ())
[%%expect{|
val dog : < bark : 'a -> unit > t as 'a = <abstr>
|}]

(* This variant from Gabriel Scherer was already wrong in OCaml 5.4,
   and has been fixed at the same time as the other two. *)
let dog : 'this =
  let open struct
    external make
      : bark:('self -> unit)
        -> < bark : ('self -> unit) > t = "%identity"
  end in
  make ~bark:(fun (o : 'this) -> ())
[%%expect{|
val dog : < bark : 'a -> unit > t as 'a = <abstr>
|}]

(* </end of #14554> *)
