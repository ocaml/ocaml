(* TEST
 * expect
*)



module type many_t = sig
  include sig
    type t (* t/3 *)
    type a = t option (* a/2 *)
    val a : a
  end

  include sig
    type t (* t/2 *)
    val eph: t
  end

  include sig
    type t (* t *)
    type b = t option
    val b : b
  end

  val eph : unit

  (* this last definition breaks the anchorability of type of a *)
  type a (* a *)
end
[%%expect {|
Lines 1-23, characters 21-3:
 1 | .....................sig
 2 |   include sig
 3 |     type t (* t/3 *)
 4 |     type a = t option (* a/2 *)
 5 |     val a : a
...
20 |
21 |   (* this last definition breaks the anchorability of type of a *)
22 |   type a (* a *)
23 | end
Error: Illegal signature: the value a has no valid type
       once the following type(s) are shadowed: a/2, t/3.
Lines 2-6, characters 2-5:
2 | ..include sig
3 |     type t (* t/3 *)
4 |     type a = t option (* a/2 *)
5 |     val a : a
6 |   end
  The type a/2 is introduced by this include.
Lines 2-6, characters 2-5:
2 | ..include sig
3 |     type t (* t/3 *)
4 |     type a = t option (* a/2 *)
5 |     val a : a
6 |   end
  The type t/3 is introduced by this include.
Line 5, characters 4-13:
5 |     val a : a
        ^^^^^^^^^
  The value a is defined here.
Lines 8-11, characters 2-5:
 8 | ..include sig
 9 |     type t (* t/2 *)
10 |     val eph: t
11 |   end
  The type t/3 is shadowed by t/2 here.
Line 22, characters 2-8:
22 |   type a (* a *)
       ^^^^^^
  The type a/2 is shadowed by a here.
|}]
