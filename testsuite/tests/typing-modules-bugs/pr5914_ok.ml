(* TEST
 expect;
*)

type 't a = [ `A ]
type 't wrap = 't constraint 't = [> 't wrap a ]
type t = t a wrap

[%%expect{|
type 't a = [ `A ]
type 'a wrap = 'a constraint 'a = [> 'a wrap a ]
type t = t a wrap
|}]

module T = struct
  let foo : 't wrap -> 't wrap -> unit = fun _ _ -> ()
  let bar : ('a a wrap as 'a) = `A
end

module Good : sig
  val bar: t
  val foo: t -> t -> unit
end = T

module Bad : sig
  val foo: t -> t -> unit
  val bar: t
end = T

[%%expect{|
module T :
  sig
    val foo : ([> 'a a ] as 'a) wrap -> 'a wrap -> unit
    val bar : [ | 'a a ] as 'a
  end
module Good : sig val bar : t val foo : t -> t -> unit end
module Bad : sig val foo : t -> t -> unit val bar : t end
|}, Principal{|
module T :
  sig
    val foo : ([> 'a wrap a ] as 'a) wrap -> 'a wrap -> unit
    val bar : [ | 'a a ] as 'a
  end
module Good : sig val bar : t val foo : t -> t -> unit end
module Bad : sig val foo : t -> t -> unit val bar : t end
|}]
