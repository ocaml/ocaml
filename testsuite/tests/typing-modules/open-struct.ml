type t = A
[%%expect{|
type t = A
|}]

module M = struct
    open struct type t' = t end
    type t = B of t * t' | C
end
[%%expect{|
module M : sig type t = B of t * t | C end
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

open struct
  open struct let counter = ref 0 end
  let inc () = incr counter
  let dec () = decr counter
  let current () = !counter
end
[%%expect{|
|}]

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
module A : sig  end
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
Line _, characters 2-74:
Error: The module identifier M#10 cannot be eliminated from val g :
                                                              M#10.M#11.t
|}]

module type S = sig open struct type t = T end val x : t end
[%%expect{|
Line _, characters 20-46:
Error: The module identifier M#12 cannot be eliminated from val x : M#12.t
|}];;


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

module M = struct
  open struct
    open struct
      module type S = sig open struct type t = int end val x : t end
      module M : S = struct let x = 1 end
    end
  end
end
[%%expect{|
module M : sig  end
|}]

module N = struct
  open struct
    module type S = sig open struct type t = T end val x : t end
  end
end
[%%expect{|
Line _, characters 24-50:
Error: The module identifier M#31 cannot be eliminated from val x : M#31.t
|}]
