(* TEST
   flags = "-dlambda -dno-locations -dcanonical-ids";
   expect;
*)
module Gmp = struct
  type t [@@shape [custom]]

  (* imaginary externals, for the sake of the example *)
  external of_int : (int[@untagged]) -> t = "gmp_of_int" "gmp_of_int_tagged"
  external add : t -> t -> t = "gmp_add"
  external add_int : t -> (int[@untagged]) -> t = "gmp_add_int" "gmp_add_int_tagged"
end
[%%expect {|
(apply (field_mut 1 (global Toploop!)) "Gmp/285" (makeblock 0))
module Gmp :
  sig
    type t [@@shape [custom]]
    external of_int : (int [@untagged]) -> t = "gmp_of_int"
      "gmp_of_int_tagged"
    external add : t -> t -> t = "gmp_add"
    external add_int : t -> (int [@untagged]) -> t = "gmp_add_int"
      "gmp_add_int_tagged"
  end
|}]

(* imaginary external, for the sake of the example *)
external add_would_overflow : int -> int -> bool = "int_add_would_overflow"
[%%expect {|
0
external add_would_overflow : int -> int -> bool = "int_add_would_overflow"
|}]

type zarith =
  | Small of int [@unboxed]
  | Big of Gmp.t [@unboxed]
[%%expect {|
0
type zarith = Small of int [@unboxed] | Big of Gmp.t [@unboxed]
|}]

let add z1 z2 =
  match z1, z2 with
  | Big gmp1, Big gmp2 ->
      Big (Gmp.add gmp1 gmp2)
  | Big gmp, Small n | Small n, Big gmp ->
      Big (Gmp.add_int gmp n)
  | Small n1, Small n2 ->
      if add_would_overflow n1 n2 then
        Big (Gmp.add_int (Gmp.of_int n1) n2)
      else
        Small (n1 + n2)
[%%expect {|
(let
  (add/0 =
     (function z1/0 z2/0
       (catch
         (if (isint z1/0)
           (if (isint z2/0)
             (if (int_add_would_overflow z1/0 z2/0)
               (gmp_add_int (gmp_of_int z1/0) z2/0) (+ z1/0 z2/0))
             (exit 2 z2/0 z1/0))
           (if (isint z2/0) (exit 2 z1/0 z2/0) (gmp_add z1/0 z2/0)))
        with (2 gmp/0 n/0[int]) (gmp_add_int gmp/0 n/0))))
  (apply (field_mut 1 (global Toploop!)) "add" add/0))
Line 1:
Error: The external function "int_add_would_overflow" is not available
|}] (* a missing external error is expected, the point is to look at the [add] code. *)

