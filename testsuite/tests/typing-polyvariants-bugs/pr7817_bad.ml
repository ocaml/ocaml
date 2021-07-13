(* TEST
   * expect
*)

let r = ref None

module M : sig
  val write : ([< `A of string | `B of int ] -> unit)
end = struct
  let write x =
    match x with `A _ | `B _ -> r := Some x
end
[%%expect{|
val r : '_weak1 option ref = {contents = None}
Lines 5-8, characters 6-3:
5 | ......struct
6 |   let write x =
7 |     match x with `A _ | `B _ -> r := Some x
8 | end
Error: Signature mismatch:
       Modules do not match:
         sig val write : _[< `A of '_weak2 | `B of '_weak3 ] -> unit end
       is not included in
         sig val write : [< `A of string | `B of int ] -> unit end
       Values do not match:
         val write : _[< `A of '_weak2 | `B of '_weak3 ] -> unit
       is not included in
         val write : [< `A of string | `B of int ] -> unit
       The type _[< `A of '_weak2 | `B of '_weak3 ] -> unit
       is not compatible with the type [< `A of string | `B of int ] -> unit
       Type _[< `A of '_weak2 | `B of '_weak3 ] is not compatible with type
         [< `A of string | `B of int ]
|}]
