type t = A

module M = struct
    open struct type t' = t end
    type t = B of t * t' | C
end

(* test *)
include struct
  open M
  let test = B (B (C, A), A)
end
[%%expect{|
type t = A
module M : sig type t = B of t * t | C end
val test : M.t = M.B (M.B (M.C, A), A)
|}];;

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

module type S = sig
  open struct
    open struct
      type t' = char
    end
    type t = t' -> int end
  val x : t
end

module M : S = struct
  let x = Char.code
end
[%%expect{|
module type S = sig val x : char -> int end
module M : S
|}];;

open struct
  open struct let counter = ref 0 end
  let inc () = incr counter
  let dec () = decr counter
  let current () = !counter
end

let () =
  inc(); inc(); dec ();
  assert (current () = 1)
[%%expect{|
|}];;

include struct open struct type t = T end let x = T end
[%%expect{|
Line _, characters 15-41:
Error: The module identifier M#7 cannot be eliminated from val x : M#7.t
|}];;

module type S = sig open struct assert false end end;;
[%%expect{|
module type S = sig  end
|}];;

module type S = sig open struct type t = int end val x : t end;;
[%%expect{|
module type S = sig val x : int end
|}];;

module type S = sig
  open struct type t = int end
  type s = t
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
