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

module M = struct
  type 'a t =
    | A of {x : 'a}
    | B: {u : 'b} -> unit t

  exception Foo of {x : int}
end;;

module N : sig
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'bla} -> unit t

  exception Foo of {x : int}
end = struct
  type 'b t = 'b M.t =
    | A of {x : 'b}
    | B: {u : 'z} -> unit t

  exception Foo = M.Foo
end;;


module type S = sig exception A of {x:int}  end;;

module F (X : sig val x : (module S) end) = struct
  module A = (val X.x)
end;;


module type S = sig
  exception A of {x : int}
  exception A of {x : string}
end;;

module M = struct
  exception A of {x : int}
  exception A of {x : string}
end;;


module M1 = struct
  exception A of {x : int}
end;;

module M = struct
  include M1
  include M1
end;;


module type S1 = sig
  exception A of {x : int}
end;;

module type S = sig
  include S1
  include S1
end;;

module M = struct
  exception A = M1.A
  exception A
end;;

module X1 = struct
  type t = ..
end;;
module X2 = struct
  type t = ..
end;;
module Z = struct
  type X1.t += A of {x: int}
  type X2.t += A of {x: int}
end;;
module Z = struct
  type X1.t += A of {x: int}
  let f = function A r -> r | _ -> assert false

  type t = A of {x: int}
  let g = function A r -> r
end;;
