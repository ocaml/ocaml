(* TEST
 expect;
*)

type 't a = [ `A ]
type 't wrap = 't constraint 't = [> 't wrap a ]
type t = t a wrap

[%%expect{|
type 't a = [ `A ]
type 'a wrap = 'a constraint 'a = [> 'a wrap a ]
Line 3, characters 0-17:
3 | type t = t a wrap
    ^^^^^^^^^^^^^^^^^
Error: The definition of "t" contains a cycle:
         "t" = "t a wrap",
         "t a wrap" = "t a",
         "t a" contains "t"
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
    val bar : [ 'a a ] as 'a
  end
Line 7, characters 11-12:
7 |   val bar: t
               ^
Error: Unbound type constructor "t"
|}, Principal{|
module T :
  sig
    val foo : ([> 'a wrap a ] as 'a) wrap -> 'a wrap -> unit
    val bar : [ 'a a ] as 'a
  end
Line 7, characters 11-12:
7 |   val bar: t
               ^
Error: Unbound type constructor "t"
|}]
