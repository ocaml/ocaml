(* TEST
   * expect
*)

[@@@ocaml.warning "+a"]

module M : sig end = struct
  module rec Foo : sig
    type t
    val create : Bar.t -> t
  end = struct
    type t = unit

    let create _ = ()
  end

  and Bar : sig
    type t
  end = struct
    type t = unit
  end

  let _ = Foo.create
end;;
[%%expect{|
module M : sig end
|}];;

module Dummy = struct end

module M : sig end = struct
  module rec Foo : sig
    val foo :
      Bar.used_1 ->
      Bar.Used_3.used_4 ->
      Bar.Functor(Dummy).used_10 ->
      Baz.B.used_outer_13 ->
      Baz.B.Used_outer_18.used_19 ->
      Baz.used_23 ->
      Baz.used_25 ->
      Baz.used_27
  end = struct
    let foo _ = assert false
    let _ = Baz.A.a
  end

  and Bar : sig
    type used_1
    type unused_2
    module Used_3 : sig
      type used_4
      type unused_5
    end
    module Unused_6 : sig
      type unused_7
    end
    val unused_8 : int
    module Functor (_ : sig end) : sig
      type used_10
      type unused_11
    end
  end = struct
    type used_1
    type unused_2
    module Used_3 = struct
      type used_4
      type unused_5
    end
    module Unused_6 = struct
      type unused_7
    end
    let unused_8 = 0
    module Functor (_ : sig end) = struct
      type used_10
      type unused_11
    end
  end

  and Baz : sig
    module rec A : sig
      val a :
        B.used_inner_12 ->
        B.Used_inner_15.used_16
    end
    and B : sig
      type used_inner_12
      type used_outer_13
      type unused_14
      module Used_inner_15 : sig
        type used_16
        type unused_17
      end
      module Used_outer_18 : sig
        type used_19
        type unused_20
      end
      module Unused_21 : sig
        type unused_22
      end
    end

    class used_23 : object end
    class unused_24 : object end

    class type used_25 = object end
    class type unused_26 = object end

    include sig
      type used_27
      type unused_28
    end

  end = struct
    module rec A : sig
      val a :
        B.used_inner_12 ->
        B.Used_inner_15.used_16
    end = struct
      let a _ = assert false
    end

    and B : sig
      type used_inner_12
      type used_outer_13
      type unused_14
      module Used_inner_15 : sig
        type used_16
        type unused_17
      end
      module Used_outer_18 : sig
        type used_19
        type unused_20
      end
      module Unused_21 : sig
        type unused_22
      end
    end = struct
      type used_inner_12
      type used_outer_13
      type unused_14
      module Used_inner_15 = struct
        type used_16
        type unused_17
      end
      module Used_outer_18 = struct
        type used_19
        type unused_20
      end
      module Unused_21 = struct
        type unused_22
      end
    end

    class used_23 = object end
    class unused_24 = object end

    class type used_25 = object end
    class type unused_26 = object end

    include struct
      type used_27
      type unused_28
    end
  end

  let _ = Foo.foo
end;;
[%%expect{|
module Dummy : sig end
Line 21, characters 4-17:
21 |     type unused_2
         ^^^^^^^^^^^^^
Warning 34: unused type unused_2.
Line 24, characters 6-19:
24 |       type unused_5
           ^^^^^^^^^^^^^
Warning 34: unused type unused_5.
Line 27, characters 6-19:
27 |       type unused_7
           ^^^^^^^^^^^^^
Warning 34: unused type unused_7.
Lines 26-28, characters 4-7:
26 | ....module Unused_6 : sig
27 |       type unused_7
28 |     end
Warning 60: unused module Unused_6.
Line 32, characters 6-20:
32 |       type unused_11
           ^^^^^^^^^^^^^^
Warning 34: unused type unused_11.
Line 60, characters 6-20:
60 |       type unused_14
           ^^^^^^^^^^^^^^
Warning 34: unused type unused_14.
Line 63, characters 8-22:
63 |         type unused_17
             ^^^^^^^^^^^^^^
Warning 34: unused type unused_17.
Line 67, characters 8-22:
67 |         type unused_20
             ^^^^^^^^^^^^^^
Warning 34: unused type unused_20.
Line 70, characters 8-22:
70 |         type unused_22
             ^^^^^^^^^^^^^^
Warning 34: unused type unused_22.
Lines 69-71, characters 6-9:
69 | ......module Unused_21 : sig
70 |         type unused_22
71 |       end
Warning 60: unused module Unused_21.
Line 75, characters 4-32:
75 |     class unused_24 : object end
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34: unused type unused_24.
Line 78, characters 4-37:
78 |     class type unused_26 = object end
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34: unused type unused_26.
Line 82, characters 6-20:
82 |       type unused_28
           ^^^^^^^^^^^^^^
Warning 34: unused type unused_28.
Line 29, characters 4-22:
29 |     val unused_8 : int
         ^^^^^^^^^^^^^^^^^^
Warning 32: unused value unused_8.
module M : sig end
|}];;
