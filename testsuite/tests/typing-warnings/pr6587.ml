(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)


module A: sig val f: fpclass -> fpclass end =
  struct
    let f _ = FP_normal
  end;;
[%%expect {|
module A : sig val f : fpclass -> fpclass end
|}]

type fpclass = A ;;
[%%expect {|
type fpclass = A
|}]

module B: sig val f: fpclass -> fpclass end =
  struct
    let f A = FP_normal
  end
    ;;
[%%expect {|
Lines 2-4, characters 2-5:
2 | ..struct
3 |     let f A = FP_normal
4 |   end
Error: Signature mismatch:
       Modules do not match:
         sig val f : fpclass -> Stdlib.fpclass end
       is not included in
         sig val f : fpclass -> fpclass end
       Values do not match:
         val f : fpclass -> Stdlib.fpclass
       is not included in
         val f : fpclass -> fpclass
       The type fpclass -> Stdlib.fpclass is not compatible with the type
         fpclass -> fpclass
       Type Stdlib.fpclass is not compatible with type fpclass
|}]
