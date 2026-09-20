(* TEST
   ocamlrunparam = "l=1000000";
   expect;
*)

(* Type functions *)
type k = float
type 'a one = [ `one of 'a ]
type 'a z = [ `zero of 'a ]
type 'a two = [`two of 'a]
type 'a three = [`three of 'a]
type 'a four = [`four of 'a]

type ('rank1, 'rank2,'rank3,'dim1,'dim2,'dim3, 'parameters) sum =
  [<`zero of 'rank2 & (* scalar broadcasting *)
             [< `zero of 'rank3 * 'dim3 & 'p1 z * 'p2 one
             | `one of 'rank3 * 'dim3 & 'p1 one * 'dim2
             | `two of 'rank3 * 'dim3 & 'p1 two * 'dim2]
  | `one of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 one * 'dim1
            | `one of 'rank3 * 'dim1 * 'dim3 & 'p1 one * 'dim2 * 'dim2 ]
  | `two of 'rank2 &
            [< `zero of 'rank3 * 'dim3 & 'p1 two * 'dim1
            | `two of 'rank3 * 'dim1 * 'dim3 & 'p1 two * 'dim1 * 'dim3 ]
  ] as 'rank1
  constraint 'parameters = 'p1 * 'p2 * 'p3

type ( 'dim, 'res, 'parameters ) cross =
  [< `two of 'res & ('p2 * 'p1 z) | `three of 'res & ('p2 three * 'p1 one) ]
  as 'dim
  constraint 'parameters = 'p1 * 'p2

(* Core type *)
type (+'dim,+'rank) t
type +'x scalar = ('a one, 'b z) t constraint 'x = 'a * 'b
type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b

(* Core interface *)
module type Vec = sig
  val scalar: k -> _ scalar
  val vec2: k -> k -> _ vec2

  val (+): ('dim1,('rank1,'rank2,'rank3,'dim1,'dim2,'dim3, _) sum ) t
    -> ('dim2,'rank2) t -> ('dim3,'rank3) t

  val cross:  ( ('dim, 'dim2 * 'rank2, _ ) cross , _ one) t ->
    ('dim, _ one) t -> ('dim2, 'rank2) t
end

module F(V:Vec) = struct
  let fn v w =
    let open V in
    (cross v w)  + scalar 1.
  let loop = fn (V.vec2 0. 1.) (V.vec2 1. 0.)
end
[%%expect{|
type k = float
type 'a one = [ `one of 'a ]
type 'a z = [ `zero of 'a ]
type 'a two = [ `two of 'a ]
type 'a three = [ `three of 'a ]
type 'a four = [ `four of 'a ]
type ('rank1, 'rank2, 'rank3, 'dim1, 'dim2, 'dim3, 'a) sum = 'rank1
  constraint 'rank1 =
    [< `one of
         'rank2 &
         [< `one of 'rank3 * 'dim1 * 'dim3 & 'p1 one * 'dim2 * 'dim2
          | `zero of 'rank3 * 'dim3 & 'p1 one * 'dim1 ]
     | `two of
         'rank2 &
         [< `two of 'rank3 * 'dim1 * 'dim3 & 'p1 two * 'dim1 * 'dim3
          | `zero of 'rank3 * 'dim3 & 'p1 two * 'dim1 ]
     | `zero of
         'rank2 &
         [< `one of 'rank3 * 'dim3 & 'p1 one * 'dim2
          | `two of 'rank3 * 'dim3 & 'p1 two * 'dim2
          | `zero of 'rank3 * 'dim3 & 'p1 z * 'p2 one ] ]
  constraint 'a = 'p1 * 'p2 * 'p3
type ('dim, 'res, 'a) cross = 'dim
  constraint 'dim =
    [< `three of 'res & 'p2 three * 'p1 one | `two of 'res & 'p2 * 'p1 z ]
  constraint 'a = 'p1 * 'p2
type (+'dim, +'rank) t
type +'c scalar = ('a one, 'b z) t constraint 'c = 'a * 'b
type +'c vec2 = ('a two, 'b one) t constraint 'c = 'a * 'b
module type Vec =
  sig
    val scalar : k -> ('a * 'b) scalar
    val vec2 : k -> k -> ('a * 'b) vec2
    val ( + ) :
      ('dim1,
       ([< `one of
             'rank2 &
             [< `one of 'rank3 * 'dim1 * 'dim3 & 'a one * 'dim2 * 'dim2
              | `zero of 'rank3 * 'dim3 & 'a one * 'dim1 ]
         | `two of
             'rank2 &
             [< `two of 'rank3 * 'dim1 * 'dim3 & 'a two * 'dim1 * 'dim3
              | `zero of 'rank3 * 'dim3 & 'a two * 'dim1 ]
         | `zero of
             'rank2 &
             [< `one of 'rank3 * 'dim3 & 'a one * 'dim2
              | `two of 'rank3 * 'dim3 & 'a two * 'dim2
              | `zero of 'rank3 * 'dim3 & 'a z * 'b one ] ],
        'rank2, 'rank3, 'dim1, 'dim2, 'dim3, 'a * 'b * 'c)
       sum)
      t -> ('dim2, 'rank2) t -> ('dim3, 'rank3) t
    val cross :
      (([< `three of 'dim2 * 'rank2 & 'a three * 'b one
         | `two of 'dim2 * 'rank2 & 'a * 'b z ]
        as 'dim, 'dim2 * 'rank2, 'b * 'a)
       cross, 'c one)
      t -> ('dim, 'd one) t -> ('dim2, 'rank2) t
  end
module F :
  (V : Vec) ->
    sig
      val fn :
        (([< `three of
               'a *
               ([< `one of
                     'c z &
                     [< `one of 'd * 'a * 'e & 'f one * 'g one * 'g one
                      | `zero of 'd * 'e & 'f one * 'a ] &
                     [< `one of 'd * 'a * 'e & 'f one * 'g one * 'g one
                      | `zero of 'd * 'e & 'f one * 'a ]
                 | `two of
                     'c z &
                     [< `two of 'd * 'a * 'e & 'f two * 'a * 'e
                      | `zero of 'd * 'e & 'f two * 'a ] &
                     [< `two of 'd * 'a * 'e & 'f two * 'a * 'e
                      | `zero of 'd * 'e & 'f two * 'a ]
                 | `zero of
                     'c z &
                     [< `one of 'd * 'e & 'f one * 'g one
                      | `two of 'd * 'e & 'f two * 'g one
                      | `zero of 'd * 'e & 'f z * 'h one ] &
                     [< `one of 'd * 'e & 'f one * 'g one
                      | `two of 'd * 'e & 'f two * 'g one
                      | `zero of 'd * 'e & 'f z * 'h one ] ]
                as 'b) &
               'i three * 'j one
           | `two of 'a * 'b & 'i * 'j z ]
          as 'dim, 'a * 'b, 'j * 'i)
         cross, 'k one)
        t -> ('dim, 'l one) t -> ('e, 'd) t
      val loop : ('a one, 'b z) t
    end
|}, Principal{|
type k = float
type 'a one = [ `one of 'a ]
type 'a z = [ `zero of 'a ]
type 'a two = [ `two of 'a ]
type 'a three = [ `three of 'a ]
type 'a four = [ `four of 'a ]
type ('rank1, 'rank2, 'rank3, 'dim1, 'dim2, 'dim3, 'a) sum = 'rank1
  constraint 'rank1 =
    [< `one of
         'rank2 &
         [< `one of 'rank3 * 'dim1 * 'dim3 & 'p1 one * 'dim2 * 'dim2
          | `zero of 'rank3 * 'dim3 & 'p1 one * 'dim1 ]
     | `two of
         'rank2 &
         [< `two of 'rank3 * 'dim1 * 'dim3 & 'p1 two * 'dim1 * 'dim3
          | `zero of 'rank3 * 'dim3 & 'p1 two * 'dim1 ]
     | `zero of
         'rank2 &
         [< `one of 'rank3 * 'dim3 & 'p1 one * 'dim2
          | `two of 'rank3 * 'dim3 & 'p1 two * 'dim2
          | `zero of 'rank3 * 'dim3 & 'p1 z * 'p2 one ] ]
  constraint 'a = 'p1 * 'p2 * 'p3
type ('dim, 'res, 'a) cross = 'dim
  constraint 'dim =
    [< `three of 'res & 'p2 three * 'p1 one | `two of 'res & 'p2 * 'p1 z ]
  constraint 'a = 'p1 * 'p2
type (+'dim, +'rank) t
type +'c scalar = ('a one, 'b z) t constraint 'c = 'a * 'b
type +'c vec2 = ('a two, 'b one) t constraint 'c = 'a * 'b
module type Vec =
  sig
    val scalar : k -> ('a * 'b) scalar
    val vec2 : k -> k -> ('a * 'b) vec2
    val ( + ) :
      ('dim1,
       ([< `one of
             'rank2 &
             [< `one of 'rank3 * 'dim1 * 'dim3 & 'a one * 'dim2 * 'dim2
              | `zero of 'rank3 * 'dim3 & 'a one * 'dim1 ]
         | `two of
             'rank2 &
             [< `two of 'rank3 * 'dim1 * 'dim3 & 'a two * 'dim1 * 'dim3
              | `zero of 'rank3 * 'dim3 & 'a two * 'dim1 ]
         | `zero of
             'rank2 &
             [< `one of 'rank3 * 'dim3 & 'a one * 'dim2
              | `two of 'rank3 * 'dim3 & 'a two * 'dim2
              | `zero of 'rank3 * 'dim3 & 'a z * 'b one ] ],
        'rank2, 'rank3, 'dim1, 'dim2, 'dim3, 'a * 'b * 'c)
       sum)
      t -> ('dim2, 'rank2) t -> ('dim3, 'rank3) t
    val cross :
      (([< `three of 'dim2 * 'rank2 & 'a three * 'b one
         | `two of 'dim2 * 'rank2 & 'a * 'b z ]
        as 'dim, 'dim2 * 'rank2, 'b * 'a)
       cross, 'c one)
      t -> ('dim, 'd one) t -> ('dim2, 'rank2) t
  end
module F :
  (V : Vec) ->
    sig
      val fn :
        ([< `three of
              'b *
              ([< `one of
                    'd z &
                    [< `one of 'e * 'b * 'f & 'g one * 'h one * 'h one
                     | `zero of 'e * 'f & 'g one * 'b ] &
                    [< `one of 'e * 'b * 'f & 'g one * 'h one * 'h one
                     | `zero of 'e * 'f & 'g one * 'b ] &
                    [< `one of 'e * 'b * 'f & 'g one * 'h one * 'h one
                     | `zero of 'e * 'f & 'g one * 'b ]
                | `two of
                    'd z &
                    [< `two of 'e * 'b * 'f & 'g two * 'b * 'f
                     | `zero of 'e * 'f & 'g two * 'b ] &
                    [< `two of 'e * 'b * 'f & 'g two * 'b * 'f
                     | `zero of 'e * 'f & 'g two * 'b ] &
                    [< `two of 'e * 'b * 'f & 'g two * 'b * 'f
                     | `zero of 'e * 'f & 'g two * 'b ]
                | `zero of
                    'd z &
                    [< `one of 'e * 'f & 'g one * 'h one
                     | `two of 'e * 'f & 'g two * 'h one
                     | `zero of 'e * 'f & 'g z * 'i one ] &
                    [< `one of 'e * 'f & 'g one * 'h one
                     | `two of 'e * 'f & 'g two * 'h one
                     | `zero of 'e * 'f & 'g z * 'i one ] &
                    [< `one of 'e * 'f & 'g one * 'h one
                     | `two of 'e * 'f & 'g two * 'h one
                     | `zero of 'e * 'f & 'g z * 'i one ] ]
               as 'c) &
              'j three * 'k one
          | `two of 'b * 'c & 'j * 'k z ]
         as 'a, 'l one)
        t -> ('a, 'm one) t -> ('f, 'e) t
      val loop : ('a one, 'b z) t
    end
|}]
