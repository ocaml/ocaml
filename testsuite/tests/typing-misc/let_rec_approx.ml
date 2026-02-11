(* TEST
 expect;
*)

module M = struct type t = A | B end
let rec f () = g A
and g (x : M.t) = f ()
[%%expect{|
module M : sig type t = A | B end
val f : unit -> 'a = <fun>
val g : M.t -> 'a = <fun>
|}]

let rec f () = g 42
and g (x : string) = f ()
[%%expect{|
Line 1, characters 17-19:
1 | let rec f () = g 42
                     ^^
Error: The constant "42" has type "int" but an expression was expected of type
         "string"
|}]

let rec opt_error ?(opt : string) () = f ?opt ()
[%%expect{|
Line 1, characters 20-32:
1 | let rec opt_error ?(opt : string) () = f ?opt ()
                        ^^^^^^^^^^^^
Error: This pattern matches values of type "string"
       but a pattern was expected which matches values of type "'a option"
|}]

let rec opt_ok_f () = opt_ok_g ~foo:A ~bar:A ()
and opt_ok_g ?(foo : M.t option) ?(bar : M.t = M.A) () = opt_ok_f ()
[%%expect{|
val opt_ok_f : unit -> 'a = <fun>
val opt_ok_g : ?foo:M.t -> ?bar:M.t -> unit -> 'a = <fun>
|}]

module M : sig
  type t = private int
  val x : t
end = struct
  type t = int
  let x = 0
end

let rec f () : M.t :> int = (M.x : M.t)
[%%expect{|
module M : sig type t = private int val x : t end
val f : unit -> int = <fun>
|}]

(* Polymorphic recursion via newtypes *)

type 'a nested =
  | List of 'a list
  | Nested of 'a list nested

(* A newtype parameter gives [depth] a polymorphic approximated type. *)
let rec depth (type a) (n : a nested) : int =
  match n with
  | List _ -> 0
  | Nested n -> 1 + depth n
[%%expect {|
type 'a nested = List of 'a list | Nested of 'a list nested
val depth : 'a nested -> int = <fun>
|}]

(* Fails, without a newtype parameter the approximated type is monomorphic. *)
let rec depth (n : _ nested) : int =
  match n with
  | List _ -> 0
  | Nested n -> 1 + depth n
[%%expect {|
Line 4, characters 26-27:
4 |   | Nested n -> 1 + depth n
                              ^
Error: The value "n" has type "'a list nested"
       but an expression was expected of type "'a nested"
       The type variable "'a" occurs inside "'a list"
|}]

(* Mutual recursion works, but requires annotations on everything. *)
let rec depth : 'a. 'a nested -> int = aux

and aux (type a) (n : a nested) : int =
  match n with
  | List _ -> 0
  | Nested n -> 1 + depth n
[%%expect {|
Line 1, characters 39-42:
1 | let rec depth : 'a. 'a nested -> int = aux
                                           ^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

(* Fails, becuase [depth] isn't annotated, therefore the locally abstract
   type escapes its scope *)
let rec depth = aux

and aux (type a) (n : a nested) : int =
  match n with
  | List _ -> 0
  | Nested n -> 1 + depth n
[%%expect {|
Line 6, characters 26-27:
6 |   | Nested n -> 1 + depth n
                              ^
Error: The value "n" has type "a list nested"
       but an expression was expected of type "'a nested"
       The type constructor "a" would escape its scope
|}]

(* Expression-form newtype [fun (type a) -> ...] is also approximated. *)
let rec depth (type a) (n : a nested) : int =
  match n with
  | List _ -> 0
  | Nested n -> 1 + depth n
[%%expect {|
val depth : 'a nested -> int = <fun>
|}]

(* Francois' original example *)
let rec choose (type a) (xs : a list) (f : a -> unit) : unit =
  match xs with
  | [] -> raise (Failure "choose")
  | [ x ] -> f x
  | x :: xs ->
    (match f x with
    | () -> ()
    | exception Failure _ -> choose xs f)
[%%expect {|
val choose : 'a list -> ('a -> unit) -> unit = <fun>
|}]
