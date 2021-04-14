(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

type t = A : t;;
[%%expect {|
type t = A : t
|}]

module X1 : sig end = struct
  let _f ~x (* x unused argument *) = function
    | A -> let x = () in x
end;;
[%%expect {|
Line 2, characters 10-11:
2 |   let _f ~x (* x unused argument *) = function
              ^
Warning 27 [unused-var-strict]: unused variable x.
module X1 : sig end
|}]

module X2 : sig end = struct
  let x = 42 (* unused value *)
  let _f = function
    | A -> let x = () in x
end;;
[%%expect {|
Line 2, characters 6-7:
2 |   let x = 42 (* unused value *)
          ^
Warning 32 [unused-value-declaration]: unused value x.
module X2 : sig end
|}]

module X3 : sig end = struct
  module O = struct let x = 42 (* unused *) end
  open O (* unused open *)

  let _f = function
    | A -> let x = () in x
end;;
[%%expect {|
Line 2, characters 24-25:
2 |   module O = struct let x = 42 (* unused *) end
                            ^
Warning 32 [unused-value-declaration]: unused value x.
Line 3, characters 2-8:
3 |   open O (* unused open *)
      ^^^^^^
Warning 33 [unused-open]: unused open O.
module X3 : sig end
|}]
