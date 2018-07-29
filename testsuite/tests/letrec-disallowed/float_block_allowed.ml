(* TEST
   * no-flat-float-array
   ** expect
*)

(* See float_block_disallowed.ml for explanations.

   When the -flat-float-array optimization is *not* set, float arrays
   are not unboxed, and no dynamic check is performed on generc array
   creation, so array literals behave just like other constructors
   and can be defined mutually recursively with their elements.
*)

(* Case of elements known not to be float. *)
let f (z: int) = let rec x = [| y; z |] and y = z in x;;
let f (z: bytes) = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
val f : int -> int array = <fun>
val f : bytes -> bytes array = <fun>
|}];;

(* Generic case (element may or may not be float), no dynamic test. *)
let f z = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
val f : 'a -> 'a array = <fun>
|}]

(* Float case, no unboxing. *)
let f (z: float) = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
val f : float -> float array = <fun>
|}]
