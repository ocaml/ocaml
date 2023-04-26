(* TEST
 expect;
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

let g1 (x : < m : 'a. <n : 'r. ([< `A of 'a * 'c] as 'r) -> unit > as 'c >) =
  x#m;;
[%%expect{|
val g1 :
  < m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > as 'b > ->
  (< n : 'e. ([< `A of 'f * 'd ] as 'e) -> unit > as 'd) = <fun>
|}]

let g2 (x : < m : 'a. <n : 'r. ([< `A of 'a * 'c] as 'r) -> unit > > as 'c) =
  x#m;;
[%%expect{|
val g2 :
  (< m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > > as 'b) ->
  < n : 'd. ([< `A of 'e * 'b ] as 'd) -> unit > = <fun>
|}, Principal{|
val g2 :
  (< m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > > as 'b) ->
  < n : 'd.
          ([< `A of
                'e *
                (< m : 'a. < n : 'g. ([< `A of 'a * 'f ] as 'g) -> unit > >
                 as 'f) ]
           as 'd) ->
          unit > =
  <fun>
|}]

let g3 (x : < m : 'a. (< n : 'b. [< `A of 'a] as 'b > as 'c) * 'c >) = x#m;;
[%%expect{|
val g3 :
  < m : 'a. < n : 'b. [< `A of 'a ] as 'b > * < n : 'c. [< `A of 'a ] as 'c > > ->
  < n : 'd. [< `A of 'e ] as 'd > * < n : 'f. [< `A of 'e ] as 'f > = <fun>
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
  < n : ([< `A of '_weak3 ] as '_weak2) ->
        (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) > =
  <obj>
|}]

(* Since the row variable is is implicitly bound, 'a and 'b don't leak *)

let h (x : < m : 'a. <n : 'b. [< `A of 'a * 'b * 'c] > > as 'c) = x#m;;
[%%expect{|
val h :
  (< m : 'a. < n : 'd 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
  < n : 'e 'b. [< `A of 'f * 'b * 'c ] as 'e > = <fun>
|}, Principal{|
val h :
  (< m : 'a. < n : 'd 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
  < n : 'e 'b.
          [< `A of
               'f * 'b *
               (< m : 'a. < n : 'h 'b0. [< `A of 'a * 'b0 * 'g ] as 'h > >
                as 'g) ]
          as 'e > =
  <fun>
|}]

(* Since the row variable is implicitly bound, 'a doesn't leak *)

let j (x : < m : 'a. <n : 'b. [< `A of 'a ] -> 'c > > as 'c) = x#m;;
[%%expect{|
val j :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd. ([< `A of 'e ] as 'd) -> 'b > = <fun>
|}, Principal{|
val j :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd.
          ([< `A of 'e ] as 'd) ->
          (< m : 'a. < n : 'g. ([< `A of 'a ] as 'g) -> 'f > > as 'f) > =
  <fun>
|}]

let o =
  object (self : 'b)
    method m : 'a. < n : [< `A of 'a] -> 'b > =
      object method n _ = self end
  end;;
[%%expect{|
val o : < m : 'c 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b = <obj>
|}]
