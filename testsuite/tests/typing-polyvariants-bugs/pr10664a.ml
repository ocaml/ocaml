(* TEST
   * expect
*)

class idfunc =
  object
    method id : 'ab. ([< `A | `B ] as 'ab) -> 'ab = fun x -> x
  end
[%%expect{|
class idfunc : object method id : ([< `A | `B ] as 'a) -> 'a end
|}]

let act : [ `A | `B ] -> [`A] =
 fun x -> (new idfunc)#id x
[%%expect{|
Line 2, characters 10-27:
2 |  fun x -> (new idfunc)#id x
              ^^^^^^^^^^^^^^^^^
Error: This expression has type [ `A | `B ]
       but an expression was expected of type [ `A ]
       The second variant type does not allow tag(s) `B
|}]


(* Some more examples involving polymorphic methods
   and polymorphic variants *)

let f (x : < m : 'a. <n : 'r. ([< `A of 'a] as 'r) -> 'c > > as 'c) = x#m;;
[%%expect{|
val f :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd. ([< `A of 'e ] as 'd) -> 'b > = <fun>
|}, Principal{|
val f :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd.
          ([< `A of 'e ] as 'd) ->
          (< m : 'a. < n : 'g. ([< `A of 'a ] as 'g) -> 'f > > as 'f) > =
  <fun>
|}]

let o =
  object (self : 's)
    method m : 'a. < n : 'r. ([< `A of 'a] as 'r) -> 's > =
      object method n : 'r. ([< `A of _] as 'r) -> 's = fun _ -> self end
  end;;
let x = f o;;
[%%expect{|
val o : < m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b = <obj>
val x :
  < n : 'b.
          ([< `A of '_weak1 ] as 'b) ->
          (< m : 'a. < n : 'd. ([< `A of 'a ] as 'd) -> 'c > > as 'c) > =
  <obj>
|}]

let g (x : < m : 'a 'b. <n : ([< `A of 'a ] as 'b) -> 'c > > as 'c) = x#m;;
[%%expect{|
val g :
  (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'd ] -> 'b > = <fun>
|}, Principal{|
val g :
  (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'd ] ->
        (< m : 'a 'f. < n : ([< `A of 'a ] as 'f) -> 'e > > as 'e) > =
  <fun>
|}]

let o =
  object (self : 's)
    method m : 'a 'b. < n : ([< `A of 'a] as 'b) -> 's > =
      object method n _ = self end
  end;;
let y = g o;;
[%%expect{|
val o : < m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b = <obj>
val y :
  < n : _[< `A of '_weak2 ] ->
        (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) > =
  <obj>
|}]

(* Since the row variable is not explicitly bound, 'a and 'b leak *)

let h (x : < m : 'a. <n : 'b. [< `A of 'a * 'b * 'c] > > as 'c) = x#m;; 
[%%expect{|
Line 1:
Error: Values do not match:
         val h :
           (< m : 'a. < n : 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
           < n : 'b. 'd >
       is not included in
         val h :
           (< m : 'a. < n : 'b. [< `A of 'e * 'b0 * 'c ] as 'd > > as 'c) ->
           < n : 'b0. 'd >
       The type
         (< m : 'a. < n : 'b. [< `A of 'e * 'b0 * 'c ] as 'd > > as 'c) ->
         < n : 'b0. 'd >
       is not compatible with the type
         (< m : 'a. < n : 'b. [< `A of 'e * 'b0 * 'f ] as 'g > > as 'f) ->
         < n : 'b0. 'g >
       Types for tag `A are incompatible
|}, Principal{|
Line 1:
Error: Values do not match:
         val h :
           (< m : 'a. < n : 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
           < n : 'b. 'd >
       is not included in
         val h :
           < m : 'a.
                   < n : 'b.
                           [< `A of 'd * 'b0 * < m : 'a0. < n : 'b1. 'c > > ]
                           as 'c > > ->
           < n : 'b0. 'c >
       The type
         < m : 'a.
                 < n : 'b.
                         [< `A of 'd * 'b0 * < m : 'a0. < n : 'b1. 'c > > ]
                         as 'c > > ->
         < n : 'b0. 'c >
       is not compatible with the type
         < m : 'a.
                 < n : 'b.
                         [< `A of 'd * 'b0 * < m : 'a0. < n : 'b1. 'e > > ]
                         as 'e > > ->
         < n : 'b0. 'e >
       Types for tag `A are incompatible
|}]

(* Since the row variable is not bound, 'a leaks *)

let j (x : < m : 'a. <n : 'b. [< `A of 'a ] -> 'c > > as 'c) = x#m;;
[%%expect{|
val j :
  (< m : 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) -> < n : 'c -> 'b > =
  <fun>
|}, Principal{|
Line 1:
Error: Values do not match:
         val j :
           (< m : 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
           < n : 'c -> (< m : 'a. < n : 'c -> 'd > > as 'd) >
       is not included in
         val j :
           (< m : 'a. < n : ([< `A of 'a0 ] as 'c) -> 'b > > as 'b) ->
           < n : 'c -> (< m : 'a0. < n : 'c -> 'd > > as 'd) >
       The type
         (< m : 'a. < n : ([< `A of 'a0 ] as 'c) -> 'b > > as 'b) ->
         < n : 'c -> (< m : 'a0. < n : 'c -> 'd > > as 'd) >
       is not compatible with the type
         (< m : 'a. < n : ([< `A of 'a0 ] as 'f) -> 'e > > as 'e) ->
         < n : 'f -> (< m : 'a0. < n : 'f -> 'g > > as 'g) >
       Types for tag `A are incompatible
|}]

let o =
  object (self : 'b)
    method m : 'a. < n : [< `A of 'a] -> 'b > =
      object method n _ = self end
  end;;
let z = j o;;
[%%expect{|
val o : < m : 'a. < n : [< `A of 'a ] -> 'b > > as 'b = <obj>
Line 6, characters 8-9:
6 | let z = j o;;
            ^
Error: This expression has type
         (< m : 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
         < n : 'c -> 'b >
       but an expression was expected of type 'd
       The universal variable 'a would escape its scope
|}, Principal{|
val o : < m : 'a. < n : [< `A of 'a ] -> 'b > > as 'b = <obj>
Line 6, characters 8-9:
6 | let z = j o;;
            ^
Error: Unbound value j
|}]
