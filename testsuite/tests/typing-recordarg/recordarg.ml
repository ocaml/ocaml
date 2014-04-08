module M = struct
  type t = A of {x:int}
  let f (A r) = r
end;;
M.f;;

module A : sig
  type t = A of {x:int}
  val f: t -> t.A
end = struct
  type t = A of {x:int}
  let f (A r) = r
end;;

module type S = sig type t = A of {x:int}  val f: t -> t.A end;;
module N : S with type t = M.t = M;;


type 'a t = A: {x : 'a; y : 'b} -> 'a t;;
let f r = A r;;

(*
module M = struct
  type 'a t =
    | A of {x : 'a}
    | B: {u : 'b} -> unit t

  exception Foo of {x : int}
end;;

module N : sig
  exception Foo of {x : int}
end = struct
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'z} -> unit t

  exception Foo = M.Foo
end;;
*)
