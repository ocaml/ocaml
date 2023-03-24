(* TEST
   * expect
*)

let x = ref None

class test =
object
  method b v = x := Some v
end

[%%expect{|
val x : '_weak1 option ref = {contents = None}
Lines 3-6, characters 0-3:
3 | class test =
4 | object
5 |   method b v = x := Some v
6 | end
Error: The type of this class,
       class test : object method b : '_weak1 -> unit end,
       contains the non-generalizable type variable(s): '_weak1.
       (see manual section 6.1.2)
|}]
