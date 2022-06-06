(* TEST
   * expect
*)

type t = A
[%%expect{|
type t = A
|}]

module M = struct
    open struct type t' = t end
    type t = B of t * t' | C
end
[%%expect{|
module M : sig type t = B of t/1 * t/2 | C end
|}]

(* test *)
include struct
  open M
  let test = B (B (C, A), A)
end
[%%expect{|
val test : M.t = M.B (M.B (M.C, A), A)
|}]

include struct
  open struct let aux x y = x / y end
  let f x = aux x 2
  let g y = aux 3 y
end
[%%expect{|
val f : int -> int = <fun>
val g : int -> int = <fun>
|}];;

include struct
  open struct exception Interrupt end
  let run () =
    raise Interrupt
  let () =
    match run() with exception Interrupt -> () | _ -> assert false
end
[%%expect{|
val run : unit -> 'a = <fun>
|}];;

(* It was decided to not allow this anymore. *)
(*
module type S = sig
  open struct
    open struct
      type t' = char
    end
  type t = t' -> int
  end
  val x : t
end
[%%expect{|
module type S = sig val x : char -> int end
|}];;

module M : S = struct
  let x = Char.code
end
[%%expect{|
module M : S
|}];;
*)

module M = struct
  module M (F: sig end) (X: sig end) = struct end
  open M(struct end)
end
[%%expect{|
Line 3, characters 7-20:
3 |   open M(struct end)
           ^^^^^^^^^^^^^
Error: This module is not a structure; it has type
       functor (X : sig end) -> sig end
|}]

open struct
  open struct let counter = ref 0 end
  let inc () = incr counter
  let dec () = decr counter
  let current () = !counter
end
[%%expect{|
val inc : unit -> unit = <fun>
val dec : unit -> unit = <fun>
val current : unit -> int = <fun>
|}]

let () =
  inc(); inc(); dec ();
  assert (current () = 1)
[%%expect{|
|}];;

include struct open struct type t = T end let x = T end
[%%expect{|
Line 1, characters 15-41:
1 | include struct open struct type t = T end let x = T end
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type t/338 introduced by this open appears in the signature
       Line 1, characters 46-47:
         The value x has no valid type if t/338 is hidden
|}];;

module A = struct
  open struct
    open struct
      type t = T
      let x = T
    end
    let y = x
  end
end
[%%expect{|
Lines 3-6, characters 4-7:
3 | ....open struct
4 |       type t = T
5 |       let x = T
6 |     end
Error: The type t/343 introduced by this open appears in the signature
       Line 7, characters 8-9:
         The value y has no valid type if t/343 is hidden
|}];;

module A = struct
  open struct
    open struct
      type t = T
    end
    let y = T
  end
  let g = y
end
[%%expect{|
Lines 3-5, characters 4-7:
3 | ....open struct
4 |       type t = T
5 |     end
Error: The type t/348 introduced by this open appears in the signature
       Line 6, characters 8-9:
         The value y has no valid type if t/348 is hidden
|}]

(* It was decided to not allow this anymore. *)
(*
module type S = sig open struct type t = T end val x : t end
[%%expect{|
Line _, characters 20-46:
  module type S = sig open struct type t = T end val x : t end
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The module identifier M#13 cannot be eliminated from val x : M#13.t
|}];;
*)


(* It was decided to not allow this anymore. *)
(*
module type S = sig
  open struct
    type t = int
    open struct
      type s = T | A of t
    end
    val x : char
  end
  val y : t
end
[%%expect{|
module type S = sig val y : int end
|}]
*)

(* It was decided to not allow this anymore. *)
(*
module type S = sig open struct assert false end end;;
[%%expect{|
module type S = sig  end
|}];;
*)

(* It was decided to not allow this anymore. *)
(*
module type S = sig open struct type t = int end val x : t end;;
[%%expect{|
module type S = sig val x : int end
|}];;
*)

module type S = sig
  (* It was decided to not allow this anymore. *)
  (*
  open struct type t = int end
  type s = t
  *)
  type s = int
end
[%%expect{|
module type S = sig type s = int end
|}]

module type T = sig type s = int end
module F(X:S) : T = X
module G(X:T) : S = X
[%%expect{|
module type T = sig type s = int end
module F : functor (X : S) -> T
module G : functor (X : T) -> S
|}]

module Counter : sig val inc : unit -> unit val current : unit -> int val z : int val zz : int end = struct
  open struct let counter = ref 0 end
  let x = 1
  let y = 2
  let dec () = decr counter

  open struct
    module A : sig val z : int end = struct
      open struct
        let n = 3
        module A = struct
          open struct
            let x = 1
          end
          let y = x
        end
        let h = A.y
        let g = A.y + n
      end
      let z = h + g
    end

    let z = 12

    module B : sig val z : int end = struct
      open struct
        module A = struct
          open struct let x = 1 end
          let y = x
          open struct let x = 1 end
          let z = y + x
        end
        let h = A.y
        let g = A.z + 1
      end
      let z = h + g
    end

    let h = A.z + B.z
  end

  let z = z + h
  let g = 1
  let ggg = 2
  let inc () = incr counter
  let zz = 5
  let current () = !counter
end
[%%expect{|
module Counter :
  sig
    val inc : unit -> unit
    val current : unit -> int
    val z : int
    val zz : int
  end
|}]

let () = begin
  assert (Counter.z = 21)
end
[%%expect{|
|}]

(* It was decided to not allow parts of this example anymore, see below for a
   slightly simpler version. *)
(*
module N = struct
  open (functor
    (N: sig open struct type t = int end val x : t end) ->
    (struct let y = N.x end))(struct let x = 1 end)

  let () =
    assert(y = 1)
end
[%%expect{|
module N : sig  end
|}]
*)
module N = struct
  open (functor
    (N: sig val x : int end) ->
    (struct let y = N.x end))(struct let x = 1 end)

  let () =
    assert(y = 1)
end
[%%expect{|
module N : sig end
|}]

module M = struct
  open struct
    open struct
      module type S = sig
        (* It was decided to not allow this anymore *)
        (* open struct type t = int end val x : t *)
        val x : int
      end
      module M : S = struct let x = 1 end
    end
  end
end
[%%expect{|
module M : sig end
|}]

(* It was decided to not allow this anymore *)
(*
module N = struct
  open struct
    module type S = sig open struct type t = T end val x : t end
  end
end
[%%expect{|
Line _, characters 24-50:
      module type S = sig open struct type t = T end val x : t end
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The module identifier M#32 cannot be eliminated from val x : M#32.t
|}]
*)

let x = let open struct open struct let y = 1 end let x = y + 1 end in x
[%%expect{|
val x : int = 2
|}]

let y =
  let
    open ((functor (X: sig val x : int end) -> struct X.x end)(struct let x = 1 end))
  in x

[%%expect{|
val y : int = 2
|}]

let x = let open struct type t = T end in T

[%%expect{|
Line 1, characters 42-43:
1 | let x = let open struct type t = T end in T
                                              ^
Error: This expression has type t but an expression was expected of type 'a
       The type constructor t would escape its scope
|}]

module type Print = sig
  type t
  val print: t -> unit
end

module Print_int: Print with type t = int = struct
  type t = int let print = print_int
end
module Print_list(P: Print): Print with type t = P.t list = struct
  type t = P.t list
  let print = List.iter P.print
end
let print_list_of_int = let open Print_list(Print_int) in print

[%%expect{|
module type Print = sig type t val print : t -> unit end
module Print_int : sig type t = int val print : t -> unit end
module Print_list :
  functor (P : Print) -> sig type t = P.t list val print : t -> unit end
val print_list_of_int : Print_int.t list -> unit = <fun>
|}]

let f () = let open functor(X: sig end) -> struct end in ();;

[%%expect{|
Line 1, characters 27-53:
1 | let f () = let open functor(X: sig end) -> struct end in ();;
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This module is not a structure; it has type
       functor (X : sig end) -> sig end
|}]
