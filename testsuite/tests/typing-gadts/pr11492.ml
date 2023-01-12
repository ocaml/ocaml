(* TEST
   * expect
*)

type _ is_int = Int : int is_int

module F (X : sig end) : sig
  type t
  val int : t is_int
end = struct
  type t = int
  let int = Int
end

module Foo = struct

  include F (struct end)

  let f : (t is_int, string) result -> string =
    fun (Error s) -> s

  let segfault () =
    print_endline (f (Ok int))

end
[%%expect {|
type _ is_int = Int : int is_int
module F : functor (X : sig end) -> sig type t val int : t is_int end
Line 16, characters 4-22:
16 |     fun (Error s) -> s
         ^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Ok Int
module Foo :
  sig
    type t
    val int : t is_int
    val f : (t is_int, string) result -> string
    val segfault : unit -> unit
  end
|}]

module Bar = struct

  module M = struct type t = int let int = Int end

  include (M : sig type t val int : t is_int end)

  let f : (t is_int, string) result -> string =
    fun (Error s) -> s

  let segfault () =
    print_endline (f (Ok int))

end
[%%expect {|
Line 8, characters 4-22:
8 |     fun (Error s) -> s
        ^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Ok Int
module Bar :
  sig
    module M : sig type t = int val int : int is_int end
    type t
    val int : t is_int
    val f : (t is_int, string) result -> string
    val segfault : unit -> unit
  end
|}]
