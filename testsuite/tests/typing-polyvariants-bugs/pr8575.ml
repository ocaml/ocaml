(* TEST
   * expect
*)

module A = struct type t = A | B let x = B end;;
[%%expect{|
module A : sig type t = A | B val x : t end
|}]

let test () =
  match A.x with
  | A as a -> `A_t a
  | B when false -> `Onoes
  | B -> if Random.bool () then `Onoes else `A_t B;;
[%%expect{|
val test : unit -> [> `A_t of A.t | `Onoes ] = <fun>
|}, Principal{|
Line 5, characters 49-50:
5 |   | B -> if Random.bool () then `Onoes else `A_t B;;
                                                     ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val test : unit -> [> `A_t of A.t | `Onoes ] = <fun>
|}]

let test () =
  match A.x with
  | B when false -> `Onoes
  | A as a -> `A_t a
  | B -> if Random.bool () then `Onoes else `A_t B;;
[%%expect{|
val test : unit -> [> `A_t of A.t | `Onoes ] = <fun>
|}, Principal{|
Line 5, characters 49-50:
5 |   | B -> if Random.bool () then `Onoes else `A_t B;;
                                                     ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val test : unit -> [> `A_t of A.t | `Onoes ] = <fun>
|}]
