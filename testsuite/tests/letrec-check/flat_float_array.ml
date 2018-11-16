(* TEST
   * flat-float-array
   ** expect
*)

(* When the -flat-float-array optimization is active (standard in
   OCaml versions up to at least 4.07), creating an array may perform
   a dynamic check, inspecing its first element to decide whether it
   is a float or not. The check is elided when the type-checker can
   determine statically that the type of the elements is float, or
   that it will never be float.

   In the dynamic check case, it is unsound to define in
   a mutually-recursive way a value and an array containing that
   value.

   In the case where an array is statically known to be an array of float,
   this dynamic check does not happen, but the elements are unboxed to
   be put in the flat float array, so they are dereferenced anyway.
*)

(* In these tests, `z` is known to be a non-float,
   so no unboxing or dynamic check happens, the definition is valid. *)
let f (z: int) = let rec x = [| y; z |] and y = z in x;;
let f (z: bytes) = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
val f : int -> int array = <fun>
val f : bytes -> bytes array = <fun>
|}];;

(* In this test, `z` has a generic/polymorphic type,
   so it could be instantiated with either float or non-float.
   A dynamic check will occur, so the definition must be rejected. *)
let f z = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
Line 1, characters 22-32:
1 | let f z = let rec x = [| y; z |] and y = z in x;;
                          ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}]

(* In this test, `z` is known to be a float, so a float array will be
   created. When the flat-float-array optimization is active, the
   array elements will be unboxed, thus evaluated. This definition
   must be rejected. *)
let f (z: float) = let rec x = [| y; z |] and y = z in x;;
[%%expect {|
Line 1, characters 31-41:
1 | let f (z: float) = let rec x = [| y; z |] and y = z in x;;
                                   ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of `let rec'
|}]
