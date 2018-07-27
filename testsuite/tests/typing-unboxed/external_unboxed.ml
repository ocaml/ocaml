(* TEST
   flags = "-strict-sequence"
   * expect
*)

(* MPR#7828 *)
type i = I of int
external id : i -> i = "%identity";;
[%%expect{|
type i = I of int
Line 2, characters 0-34:
  external id : i -> i = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61: This primitive declaration uses type i, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of i with [@@boxed]
or [@@unboxed].
Line 2, characters 0-34:
  external id : i -> i = "%identity";;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 61: This primitive declaration uses type i, which is unannotated and
unboxable. The representation of such types may change in future
versions. You should annotate the declaration of i with [@@boxed]
or [@@unboxed].
external id : i -> i = "%identity"
|}];;

type ib = I of int [@@boxed]
external idb : ib -> ib = "%identity";;
[%%expect{|
type ib = I of int
external idb : ib -> ib = "%identity"
|}];;

type iub = I of int [@@unboxed]
external idub : iub -> iub = "%identity";;
[%%expect{|
type iub = I of int [@@unboxed]
external idub : iub -> iub = "%identity"
|}];;
