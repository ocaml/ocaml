(* TEST
   * expect
*)

module rec M : sig
  type t
  val x : unit -> t
  val f : unit -> string
end = struct
  type t = int
  let x () : t = 3

  private type t = { x : string }

  let f () = (M.x () : t).x
end
;;
[%%expect{|
Line 11, characters 14-20:
    let f () = (M.x () : t).x
                ^^^^^^
Error: This expression has type M.t but an expression was expected of type t
|}]

module rec M : sig
  module N : sig
    type t
    val x : unit -> t
    val f : unit -> string
  end
end = struct
  module N = struct
    type t = int
    let x () : t = 3

  end

  private module N = struct
    type t = { x : string }
  end

  let f () = (M.N.x () : N.t).x
end
;;
[%%expect{|
Line 18, characters 14-22:
    let f () = (M.N.x () : N.t).x
                ^^^^^^^^
Error: This expression has type M.N.t but an expression was expected of type
         N.t
|}]
