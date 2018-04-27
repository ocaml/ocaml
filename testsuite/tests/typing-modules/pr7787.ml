(* TEST
   * expect
*)

module O (T : sig
    module N : sig
      val foo : int -> int
    end
  end) = struct
  open T

  let go () =
    N.foo 42 (* finding N (from T) goes wrong *)
end

module T = struct
  module N = struct
    let foo x = x + 3
  end
end;;
[%%expect{|
module O :
  functor (T : sig module N : sig val foo : int -> int end end) ->
    sig val go : unit -> int end
module T : sig module N : sig val foo : int -> int end end
|}]

(* Incidentally, M isn't used in T2, but it doesn't seem to fail if
   it's just "module M" and "module T2" separately *)
module rec M : sig
  val go : unit -> int
end = O (T2)
and T2 : sig
  include module type of struct include T end
end = struct
  include T
end;;
[%%expect{|
module rec M : sig val go : unit -> int end
and T2 : sig module N = T.N end
|}]

let () = ignore (M.go ())
[%%expect{|
|}]
