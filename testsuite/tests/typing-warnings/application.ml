(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

let _ = Array.get;;
[%%expect {|
- : 'a array -> int -> 'a = <fun>
|}]

let _ = Array.get [||];;
[%%expect {|
Line 1, characters 8-22:
1 | let _ = Array.get [||];;
            ^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : int -> 'a = <fun>
|}]

let () = ignore Array.get;;
[%%expect {|
|}]

let () = ignore (Array.get [||]);;
[%%expect {|
Line 1, characters 16-32:
1 | let () = ignore (Array.get [||]);;
                    ^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.
|}]


let _ = if true then Array.get else (fun _ _ -> 12);;
[%%expect {|
- : int array -> int -> int = <fun>
|}]

let _ = if true then Array.get [||] else (fun _ -> 12);;
[%%expect {|
Line 1, characters 21-35:
1 | let _ = if true then Array.get [||] else (fun _ -> 12);;
                         ^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : int -> int = <fun>
|}]

let _ = (if true then Array.get [||] else (fun _ -> 12) : _ -> _);;
[%%expect {|
- : int -> int = <fun>
|}]

type t = {r: int -> int -> int}

let f x = let _ = x.r in ();;
[%%expect {|
type t = { r : int -> int -> int; }
val f : t -> unit = <fun>
|}]

let f x = let _ = x.r 1 in ();;
[%%expect {|
Line 1, characters 18-23:
1 | let f x = let _ = x.r 1 in ();;
                      ^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

val f : t -> unit = <fun>
|}]

let f a b = a + b;;
match f 42 with
| _ -> ();;
[%%expect {|
val f : int -> int -> int = <fun>
Line 2, characters 6-10:
2 | match f 42 with
          ^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : unit = ()
|}]

let f a b = a + b;;
match f 42 with
| _ -> ()
| exception _ -> ();;
[%%expect {|
val f : int -> int -> int = <fun>
Line 2, characters 6-10:
2 | match f 42 with
          ^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
  maybe some arguments are missing.

- : unit = ()
|}]

let f a b = a + b;;
match f 42 with
| x -> ignore (x 34);;
[%%expect {|
val f : int -> int -> int = <fun>
- : unit = ()
|}]


let f a b = a + b;;
match (f 42 : _) with
| _ -> ();;
[%%expect {|
val f : int -> int -> int = <fun>
- : unit = ()
|}]

let _ = raise Exit 3;;
[%%expect {|
Line 1, characters 19-20:
1 | let _ = raise Exit 3;;
                       ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

Exception: Stdlib.Exit.
|}]

let f a b = a + b;;
[%%expect {|
val f : int -> int -> int = <fun>
|}]
let g x = x + 1
let _ = g (f 1);;
[%%expect {|
val g : int -> int = <fun>
Line 2, characters 10-15:
2 | let _ = g (f 1);;
              ^^^^^
Error: This expression has type "int -> int"
       but an expression was expected of type "int"
Hint: This function application is partial, maybe some arguments are missing.
|}]


[@@@warning "+20"]

(* Simple cases *)
let raise_late _ _ = assert false
let f id x = id x
[%%expect {|
val raise_late : 'a -> 'b -> 'c = <fun>
val f : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let work x y z = raise_late x y z
[%%expect {|
Line 1, characters 32-33:
1 | let work x y z = raise_late x y z
                                    ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val work : 'a -> 'b -> 'c -> 'd = <fun>
|}]

let fail x y z = f (raise_late x) y z
[%%expect {|
val fail : 'a -> 'b -> 'c -> 'd = <fun>
|}]

(* GADTS *)

type _ t = F: (int -> string) t
let print (type a) (x: a t): a = match x with
  | F -> string_of_int
[%%expect {|
type _ t = F : (int -> string) t
val print : 'a t -> 'a = <fun>
|}]

let s = print F 0
[%%expect {|
val s : string = "0"
|}]

(* Modular explicits *)

module type T = sig type 'a t end
let f (module _:T) _ = assert false
[%%expect {|
module type T = sig type 'a t end
val f : (module T) -> 'a -> 'b = <fun>
|}]


let ok = f (module List) () ()
[%%expect {|
Line 1, characters 28-30:
1 | let ok = f (module List) () ()
                                ^^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

Exception: Assert_failure ("", 2, 23).
|}]

let g (module M:T) (x:_ M.t) = x
module Raise = struct
  type 'a t = unit -> 'a
end
[%%expect {|
val g : (module M : T) -> 'a M.t -> 'a M.t = <fun>
module Raise : sig type 'a t = unit -> 'a end
|}]

let fail = g (module Raise) (fun () -> assert false) () ()
[%%expect {|
Exception: Assert_failure ("", 1, 39).
|}]


module type Arrow = sig type ('a,'b) t = 'a -> 'b end
module Arrow = struct type ('a,'b) t = 'a -> 'b end
[%%expect {|
module type Arrow = sig type ('a, 'b) t = 'a -> 'b end
module Arrow : sig type ('a, 'b) t = 'a -> 'b end
|}]

let f (module M:Arrow): ('a,'a) M.t = fun x -> x
let ok x y = f (module Arrow) x y
[%%expect {|
val f : (module Arrow) -> 'a -> 'a = <fun>
val ok : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

let f (module M:Arrow): ('a,'b) M.t = fun _ -> assert false
let ok_error x y = f (module Arrow) x y
[%%expect {|
val f : (module Arrow) -> 'a -> 'b = <fun>
Line 2, characters 38-39:
2 | let ok_error x y = f (module Arrow) x y
                                          ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

val ok_error : 'a -> 'b -> 'c = <fun>
|}]

module type T = sig type t val x:t end
let f (type a) (module M: T with type t = a) = M.x
let no_warning x y = f x y
[%%expect {|
module type T = sig type t val x : t end
val f : (module M : T with type t = 'a) -> M.t = <fun>
val no_warning : (module T with type t = 'a -> 'b) -> 'a -> 'b = <fun>
|}]

(* GADTS + modular explicits *)

module type Show = sig
  type t
  val show: t -> string
end
type (_,_) t = S: ('x, 'x -> string) t

module I = struct type t = int let show = string_of_int end

let f: type a. (module M:Show) -> (M.t,a) t -> a =
  fun (module M:Show) x -> match x with
    | S -> M.show
[%%expect {|
module type Show = sig type t val show : t -> string end
type (_, _) t = S : ('x, 'x -> string) t
module I : sig type t = int val show : int -> string end
val f : (module M : Show) -> (M.t, 'a) t -> 'a = <fun>
|}]

let s = f (module I) S 0
[%%expect {|
val s : string = "0"
|}]
