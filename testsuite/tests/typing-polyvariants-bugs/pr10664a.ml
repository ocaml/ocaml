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
Error: This expression has type "[ `A | `B ]"
       but an expression was expected of type "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]


(* Some more examples involving polymorphic methods
   and polymorphic variants *)

let f (x : < m : 'a. <n : 'r. ([< `A of 'a] as 'r) -> 'c > > as 'c) = x#m;;
[%%expect{|
val f :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd. ([< `A of 'a ] as 'd) -> 'b > = <fun>
|}, Principal{|
val f :
  (< m : 'a. < n : 'c. ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : 'd.
          ([< `A of 'a ] as 'd) ->
          (< m : 'c. < n : 'f. ([< `A of 'c ] as 'f) -> 'e > > as 'e) > =
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
          ([< `A of '_a ] as 'b) ->
          (< m : 'd. < n : 'e. ([< `A of 'd ] as 'e) -> 'c > > as 'c) > =
  <obj>
|}]

let g1 (x : < m : 'a. <n : 'r. ([< `A of 'a * 'c] as 'r) -> unit > as 'c >) =
  x#m;;
[%%expect{|
val g1 :
  < m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > as 'b > ->
  (< n : 'e. ([< `A of 'a * 'd ] as 'e) -> unit > as 'd) = <fun>
|}]

let g2 (x : < m : 'a. <n : 'r. ([< `A of 'a * 'c] as 'r) -> unit > > as 'c) =
  x#m;;
[%%expect{|
val g2 :
  (< m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > > as 'b) ->
  < n : 'd. ([< `A of 'a * 'b ] as 'd) -> unit > = <fun>
|}, Principal{|
val g2 :
  (< m : 'a. < n : 'c. ([< `A of 'a * 'b ] as 'c) -> unit > > as 'b) ->
  < n : 'd.
          ([< `A of
                'a *
                (< m : 'c. < n : 'f. ([< `A of 'c * 'e ] as 'f) -> unit > >
                 as 'e) ]
           as 'd) ->
          unit > =
  <fun>
|}]

let g3 (x : < m : 'a. (< n : 'b. [< `A of 'a] as 'b > as 'c) * 'c >) = x#m;;
[%%expect{|
val g3 :
  < m : 'a. < n : 'b. [< `A of 'a ] as 'b > * < n : 'c. [< `A of 'a ] as 'c > > ->
  < n : 'd. [< `A of 'a ] as 'd > * < n : 'e. [< `A of 'a ] as 'e > = <fun>
|}]

let g (x : < m : 'a 'b. <n : ([< `A of 'a ] as 'b) -> 'c > > as 'c) = x#m;;
[%%expect{|
val g :
  (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'a ] -> 'b > = <fun>
|}, Principal{|
val g :
  (< m : 'a 'c. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'a ] ->
        (< m : 'c 'e. < n : ([< `A of 'c ] as 'e) -> 'd > > as 'd) > =
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
  < n : ([< `A of '_a ] as '_weak1) ->
        (< m : 'c 'd. < n : ([< `A of 'c ] as 'd) -> 'b > > as 'b) > =
  <obj>
|}]

(* Since the row variable is implicitly bound, 'a and 'b don't leak *)

let h (x : < m : 'a. <n : 'b. [< `A of 'a * 'b * 'c] > > as 'c) = x#m;;
[%%expect{|
val h :
  (< m : 'a. < n : 'd 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
  < n : 'e 'b. [< `A of 'a * 'b * 'c ] as 'e > = <fun>
|}, Principal{|
val h :
  (< m : 'a. < n : 'd 'b. [< `A of 'a * 'b * 'c ] as 'd > > as 'c) ->
  < n : 'e 'b.
          [< `A of
               'a * 'b *
               (< m : 'd. < n : 'g 'b0. [< `A of 'd * 'b0 * 'f ] as 'g > >
                as 'f) ]
          as 'e > =
  <fun>
|}]

(* Since the row variable is implicitly bound, 'a doesn't leak *)

let j (x : < m : 'a. <n : 'b. [< `A of 'a ] -> 'c > > as 'c) = x#m;;
[%%expect{|
val j :
  (< m : 'c 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'a ] -> 'b > = <fun>
|}, Principal{|
val j :
  (< m : 'c 'a. < n : ([< `A of 'a ] as 'c) -> 'b > > as 'b) ->
  < n : [< `A of 'a ] ->
        (< m : 'e 'c. < n : ([< `A of 'c ] as 'e) -> 'd > > as 'd) > =
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
