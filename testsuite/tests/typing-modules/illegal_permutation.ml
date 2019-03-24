(* TEST
* expect
*)
class type ct = object end
module type s = sig type a val one:int type b class two:ct type c type exn+=Three type d end
module type c12 = sig type a class two:ct type b val one:int type c type exn+=Three type d end
module type c123 = sig type a type exn+=Three type b class two:ct type c val one:int type d end

module type expected = sig module type x = s end

module A: expected = struct module type x = c12 end
[%%expect {|
class type ct = object  end
module type s =
  sig
    type a
    val one : int
    type b
    class two : ct
    type c
    type exn += Three
    type d
  end
module type c12 =
  sig
    type a
    class two : ct
    type b
    val one : int
    type c
    type exn += Three
    type d
  end
module type c123 =
  sig
    type a
    type exn += Three
    type b
    class two : ct
    type c
    val one : int
    type d
  end
module type expected = sig module type x = s end
Line 8, characters 21-51:
8 | module A: expected = struct module type x = c12 end
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c12 end
       is not included in
         expected
       Module type declarations do not match:
         module type x = c12
       does not match
         module type x = s
       At position module type x = <here>
       Illegal permutation of structure fields:
         the class "two" and the value "one" are swapped.
|}]

module B: expected = struct module type x = c123 end
[%%expect {|
Line 1, characters 21-52:
1 | module B: expected = struct module type x = c123 end
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = c123 end
       is not included in
         expected
       Module type declarations do not match:
         module type x = c123
       does not match
         module type x = s
       At position module type x = <here>
       Illegal permutation of structure fields:
         the extension constructor "Three" and the value "one" are swapped.
|}]


module Far: sig
  module type x = sig
    val a:int
    val b: int
    val c: int
    val d: int
    val e:int
  end
end = struct
  module type x = sig
    val a:int
    val b:int
    val e:int
    val d:int
    val c:int
  end
end
[%%expect {|
Line 9, characters 6-114:
 9 | ......struct
10 |   module type x = sig
11 |     val a:int
12 |     val b:int
13 |     val e:int
14 |     val d:int
15 |     val c:int
16 |   end
17 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               val a : int
               val b : int
               val e : int
               val d : int
               val c : int
             end
         end
       is not included in
         sig
           module type x =
             sig
               val a : int
               val b : int
               val c : int
               val d : int
               val e : int
             end
         end
       Module type declarations do not match:
         module type x =
           sig
             val a : int
             val b : int
             val e : int
             val d : int
             val c : int
           end
       does not match
         module type x =
           sig
             val a : int
             val b : int
             val c : int
             val d : int
             val e : int
           end
       At position module type x = <here>
       Illegal permutation of structure fields:
         the value "e" and the value "c" are swapped.
|}]

module Confusing: sig
  module type x= sig
    class x:ct
    val x:int
  end
end = struct
  module type x= sig
    val x:int
    class x:ct
  end
end
[%%expect {|
Line 6, characters 6-72:
 6 | ......struct
 7 |   module type x= sig
 8 |     val x:int
 9 |     class x:ct
10 |   end
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig val x : int class x : ct end end
       is not included in
         sig module type x = sig class x : ct val x : int end end
       Module type declarations do not match:
         module type x = sig val x : int class x : ct end
       does not match
         module type x = sig class x : ct val x : int end
       At position module type x = <here>
       Illegal permutation of structure fields:
         the value "x" and the class "x" are swapped.
|}]

module MT: sig
  module type a = sig
    module type b = sig
      val x:int
      val y:int
    end
  end
end = struct
  module type a = sig
    module type b = sig
      val y:int
      val x:int
    end
  end
end
[%%expect {|
Line 8, characters 6-108:
 8 | ......struct
 9 |   module type a = sig
10 |     module type b = sig
11 |       val y:int
12 |       val x:int
13 |     end
14 |   end
15 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type a =
             sig module type b = sig val y : int val x : int end end
         end
       is not included in
         sig
           module type a =
             sig module type b = sig val x : int val y : int end end
         end
       Module type declarations do not match:
         module type a =
           sig module type b = sig val y : int val x : int end end
       does not match
         module type a =
           sig module type b = sig val x : int val y : int end end
       At position module type a = <here>
       Modules do not match:
         sig module type b = sig val y : int val x : int end end
       is not included in
         sig module type b = sig val x : int val y : int end end
       At position module type a = <here>
       Module type declarations do not match:
         module type b = sig val y : int val x : int end
       does not match
         module type b = sig val x : int val y : int end
       At position module type a = sig module type b = <here> end
       Illegal permutation of structure fields:
         the value "y" and the value "x" are swapped.
|}]

class type ct = object end
module Classes: sig
  module type x = sig
    class a: ct
    class b: ct
  end
end = struct
  module type x = sig
    class b: ct
    class a: ct
  end
end
[%%expect{|
class type ct = object  end
Line 7, characters 6-76:
 7 | ......struct
 8 |   module type x = sig
 9 |     class b: ct
10 |     class a: ct
11 |   end
12 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig class b : ct class a : ct end end
       is not included in
         sig module type x = sig class a : ct class b : ct end end
       Module type declarations do not match:
         module type x = sig class b : ct class a : ct end
       does not match
         module type x = sig class a : ct class b : ct end
       At position module type x = <here>
       Illegal permutation of structure fields:
         the class "b" and the class "a" are swapped.
|}]

module Ext: sig
  module type x = sig
    type exn+=A
    type exn+=B
  end
end = struct
  module type x = sig
    type exn+=B
    type exn+=A
  end
end
[%%expect{|
Line 6, characters 6-76:
 6 | ......struct
 7 |   module type x = sig
 8 |     type exn+=B
 9 |     type exn+=A
10 |   end
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = sig type exn += B type exn += A end end
       is not included in
         sig module type x = sig type exn += A type exn += B end end
       Module type declarations do not match:
         module type x = sig type exn += B type exn += A end
       does not match
         module type x = sig type exn += A type exn += B end
       At position module type x = <here>
       Illegal permutation of structure fields:
         the extension constructor "B"
         and the extension constructor "A" are swapped.
|}]


module type w = sig
  module One:s
  module Two:s
end

module type w21 = sig
  module Two:s
  module One:s
end

module type wOne21 = sig
  module One:c12
  module Two:s
end

module C: sig module type x = w end = struct module type x = w21 end
[%%expect {|
module type w = sig module One : s module Two : s end
module type w21 = sig module Two : s module One : s end
module type wOne21 = sig module One : c12 module Two : s end
Line 16, characters 38-68:
16 | module C: sig module type x = w end = struct module type x = w21 end
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = w21 end
       is not included in
         sig module type x = w end
       Module type declarations do not match:
         module type x = w21
       does not match
         module type x = w
       At position module type x = <here>
       Illegal permutation of structure fields:
         the module "Two" and the module "One" are swapped.
|}]

module D: sig module type x = w end = struct module type x = wOne21 end
[%%expect {|
Line 1, characters 38-71:
1 | module D: sig module type x = w end = struct module type x = wOne21 end
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type x = wOne21 end
       is not included in
         sig module type x = w end
       Module type declarations do not match:
         module type x = wOne21
       does not match
         module type x = w
       At position module type x = <here>
       Illegal permutation of structure fields:
         in module One,
         the class "two" and the value "one" are swapped.
|}]

module F1: sig module type x = functor(X:s) -> s end =
struct
  module type x = functor(X:c12) -> s
end
[%%expect {|
Line 2, characters 0-48:
2 | struct
3 |   module type x = functor(X:c12) -> s
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = functor (X : c12) -> s end
       is not included in
         sig module type x = functor (X : s) -> s end
       Module type declarations do not match:
         module type x = functor (X : c12) -> s
       does not match
         module type x = functor (X : s) -> s
       At position module type x = <here>
       Illegal permutation of structure fields:
         at position functor (X : <here>) -> ...,
         the class "two" and the value "one" are swapped.
|}]

module F2: sig module type x = functor(X:s) -> s end =
struct
  module type x = functor(X:s) -> c12
end
[%%expect {|
Line 2, characters 0-48:
2 | struct
3 |   module type x = functor(X:s) -> c12
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig module type x = functor (X : s) -> c12 end
       is not included in
         sig module type x = functor (X : s) -> s end
       Module type declarations do not match:
         module type x = functor (X : s) -> c12
       does not match
         module type x = functor (X : s) -> s
       At position module type x = <here>
       Illegal permutation of structure fields:
         at position functor (X) -> <here>,
         the class "two" and the value "one" are swapped.
|}]

module Nested: sig
  module type x = sig
    module A: sig
      module B: sig
        module C: functor(X:sig end)(Y:sig end)
          (Z:
           sig
             module D: sig
               module E: sig
                 module F:functor(X:sig end)
                   (Arg:sig
                      val one:int
                      val two:int
                    end) -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end=struct
  module type x = sig
    module A: sig
      module B: sig
        module C: functor(X:sig end)(Y:sig end)
          (Z:
           sig
             module D: sig
               module E: sig
                 module F:functor(X:sig end)
                   (Arg:sig
                      val two:int
                      val one:int
                    end) -> sig end
               end
             end
           end)
          -> sig end
      end
    end
  end
end
[%%expect {|
Line 22, characters 4-481:
22 | ....struct
23 |   module type x = sig
24 |     module A: sig
25 |       module B: sig
26 |         module C: functor(X:sig end)(Y:sig end)
...
40 |       end
41 |     end
42 |   end
43 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         functor
                           (X : sig  end) (Y : sig  end) (Z : sig
                                                                module D :
                                                                  sig
                                                                    module E :
                                                                    sig
                                                                    module F :
                                                                    functor
                                                                    (X :
                                                                    sig

                                                                    end) (Arg :
                                                                    sig
                                                                    val two :
                                                                    int
                                                                    val one :
                                                                    int
                                                                    end) ->
                                                                    sig  end
                                                                    end
                                                                  end
                                                              end) ->
                           sig  end
                     end
                 end
             end
         end
       is not included in
         sig
           module type x =
             sig
               module A :
                 sig
                   module B :
                     sig
                       module C :
                         functor
                           (X : sig  end) (Y : sig  end) (Z : sig
                                                                module D :
                                                                  sig
                                                                    module E :
                                                                    sig
                                                                    module F :
                                                                    functor
                                                                    (X :
                                                                    sig

                                                                    end) (Arg :
                                                                    sig
                                                                    val one :
                                                                    int
                                                                    val two :
                                                                    int
                                                                    end) ->
                                                                    sig  end
                                                                    end
                                                                  end
                                                              end) ->
                           sig  end
                     end
                 end
             end
         end
       Module type declarations do not match:
         module type x =
           sig
             module A :
               sig
                 module B :
                   sig
                     module C :
                       functor
                         (X : sig  end) (Y : sig  end) (Z : sig
                                                              module D :
                                                                sig
                                                                  module E :
                                                                    sig
                                                                    module F :
                                                                    functor
                                                                    (X :
                                                                    sig

                                                                    end) (Arg :
                                                                    sig
                                                                    val two :
                                                                    int
                                                                    val one :
                                                                    int
                                                                    end) ->
                                                                    sig  end
                                                                    end
                                                                end
                                                            end) ->
                         sig  end
                   end
               end
           end
       does not match
         module type x =
           sig
             module A :
               sig
                 module B :
                   sig
                     module C :
                       functor
                         (X : sig  end) (Y : sig  end) (Z : sig
                                                              module D :
                                                                sig
                                                                  module E :
                                                                    sig
                                                                    module F :
                                                                    functor
                                                                    (X :
                                                                    sig

                                                                    end) (Arg :
                                                                    sig
                                                                    val one :
                                                                    int
                                                                    val two :
                                                                    int
                                                                    end) ->
                                                                    sig  end
                                                                    end
                                                                end
                                                            end) ->
                         sig  end
                   end
               end
           end
       At position module type x = <here>
       Illegal permutation of structure fields:
         at position
           module A :
             sig
               module B :
                 sig
                   module C(X)(Y)(Z :
                     sig
                       module D :
                         sig
                           module E : sig module F(X)(Arg : <here>) : ... end
                         end
                     end) : ...
                 end
             end,
         the value "two" and the value "one" are swapped.
|}]

