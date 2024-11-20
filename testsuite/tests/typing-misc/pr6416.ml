(* TEST
 flags = "-no-alias-deps -w +40";
 expect;
*)
module M = struct
  type t = A
  module M : sig
    val f : t -> unit
  end = struct
    type t = B
    let f B = ()
  end
end;;
[%%expect{|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     type t = B
7 |     let f B = ()
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig type t = B val f : t -> unit end
       is not included in
         sig val f : t -> unit end
       Values do not match:
         val f : t -> unit
       is not included in
         val f : t/2 -> unit
       The type "t -> unit" is not compatible with the type "t/2 -> unit"
       Type "t" is not compatible with type "t/2"
       Line 6, characters 4-14:
         Definition of type "t"
       Line 2, characters 2-12:
         Definition of type "t/2"
|}]

module N = struct
  type t= A
  module M: sig type u = A of t end =
  struct type t = B type u = A of t end
end;;
[%%expect{|
Line 4, characters 2-39:
4 |   struct type t = B type u = A of t end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = B type u = A of t end
       is not included in
         sig type u = A of t end
       Type declarations do not match:
         type u = A of t
       is not included in
         type u = A of t/2
       Constructors do not match:
         "A of t"
       is not the same as:
         "A of t/2"
       The type "t" is not equal to the type "t/2"
       Line 4, characters 9-19:
         Definition of type "t"
       Line 2, characters 2-11:
         Definition of type "t/2"
|}]

module K = struct
  module type s
  module M: sig module A:functor(X:s) -> sig end end =
    struct
      module type s
      module A(X:s) =struct end
    end
end;;

[%%expect{|
Lines 4-7, characters 4-7:
4 | ....struct
5 |       module type s
6 |       module A(X:s) =struct end
7 |     end
Error: Signature mismatch:
       Modules do not match:
         sig module type s module A : (X : s) -> sig end end
       is not included in
         sig module A : (X : s) -> sig end end
       In module "A":
       Modules do not match:
         (X : s) -> ...
       is not included in
         (X : s/2) -> ...
       Module types do not match: s does not include s/2
Line 5, characters 6-19:
  Definition of module type "s"
Line 2, characters 2-15:
  Definition of module type "s/2"
|}]

module L = struct
  module T = struct type t end
  module M: sig type t = A of T.t end =
    struct
      module T = struct type t end
      type t = A of T.t
    end
end;;
      [%%expect {|
Lines 4-7, characters 4-7:
4 | ....struct
5 |       module T = struct type t end
6 |       type t = A of T.t
7 |     end
Error: Signature mismatch:
       Modules do not match:
         sig module T : sig type t end type t = A of T.t end
       is not included in
         sig type t = A of T.t end
       Type declarations do not match:
         type t = A of T.t
       is not included in
         type t = A of T/2.t
       Constructors do not match:
         "A of T.t"
       is not the same as:
         "A of T/2.t"
       The type "T.t" is not equal to the type "T/2.t"
       Line 5, characters 6-34:
         Definition of module "T"
       Line 2, characters 2-30:
         Definition of module "T/2"
|}]

module O = struct
  module type s
  type t = A
  module M: sig val f: (module s) -> t -> t end =
  struct module type s type t = B let f (module X:s) A = B end
end;;

[%%expect{|
Line 5, characters 2-62:
5 |   struct module type s type t = B let f (module X:s) A = B end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type s type t = B val f : (module s) -> t/2 -> t end
       is not included in
         sig val f : (module s) -> t -> t end
       Values do not match:
         val f : (module s) -> t/2 -> t
       is not included in
         val f : (module s/2) -> t/2 -> t/2
       The type "(module s) -> t/2 -> t" is not compatible with the type
         "(module s/2) -> t/2 -> t/2"
       Modules do not match: s is not included in s/2
       Line 5, characters 23-33:
         Definition of type "t"
       Line 3, characters 2-12:
         Definition of type "t/2"
       Line 5, characters 9-22:
         Definition of module type "s"
       Line 2, characters 2-15:
         Definition of module type "s/2"
|}]

module P = struct
  module type a
  type a = A
   module M : sig val f: a -> (module a) -> a  end
   = struct type a = B let f A _  = B end
end;;

[%%expect{|
Line 5, characters 5-41:
5 |    = struct type a = B let f A _  = B end
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type a = B val f : a/2 -> 'a -> a end
       is not included in
         sig val f : a -> (module a) -> a end
       Values do not match:
         val f : a/2 -> 'a -> a
       is not included in
         val f : a/2 -> (module a) -> a/2
       The type "a/2 -> (module a) -> a" is not compatible with the type
         "a/2 -> (module a) -> a/2"
       Type "a" is not compatible with type "a/2"
       Line 5, characters 12-22:
         Definition of type "a"
       Line 3, characters 2-12:
         Definition of type "a/2"
|}]

module Q = struct
  class a = object method m = () end
  module M: sig class b: a end =
  struct
    class a = object method c = let module X = struct type t end in () end
    class b = a
  end
end;;


[%%expect{|
Lines 4-7, characters 2-5:
4 | ..struct
5 |     class a = object method c = let module X = struct type t end in () end
6 |     class b = a
7 |   end
Error: Signature mismatch:
       Modules do not match:
         sig class a : object method c : unit end class b : a end
       is not included in
         sig class b : a end
       Class declarations do not match:
         class b : a
       does not match
         class b : a/2
       The public method c cannot be hidden
       The first class type has no method m
Line 5, characters 4-74:
  Definition of class type "a"
Line 2, characters 2-36:
  Definition of class type "a/2"
|}]

module R = struct
  class type a = object method m: unit end
  module M: sig class type b= a end =
  struct
    class type a = object end
    class type b = a
  end
end;;

[%%expect{|
Lines 4-7, characters 2-5:
4 | ..struct
5 |     class type a = object end
6 |     class type b = a
7 |   end
Error: Signature mismatch:
       Modules do not match:
         sig class type a = object  end class type b = a end
       is not included in
         sig class type b = a end
       Class type declarations do not match:
         class type b = a
       does not match
         class type b = a/2
       The first class type has no method m
Line 5, characters 4-29:
  Definition of class type "a"
Line 2, characters 2-42:
  Definition of class type "a/2"
|}]

module S = struct
  class a= object end
  class b = a
end;;

[%%expect{|
module S : sig class a : object  end class b : a end
|}]

module X: sig
  type t
  class type a = object method m:t end
  module K: sig
    type t
    class type c = object method m: t end
  end
end = struct
  type t
  class type a = object method m:t end
  module K = struct
    type t
    class type c = object inherit a end
  end
end;;

[%%expect{|
Lines 8-15, characters 6-3:
 8 | ......struct
 9 |   type t
10 |   class type a = object method m:t end
11 |   module K = struct
12 |     type t
13 |     class type c = object inherit a end
14 |   end
15 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t
           class type a = object method m : t end
           module K : sig type t class type c = object method m : t/2 end end
         end
       is not included in
         sig
           type t
           class type a = object method m : t end
           module K : sig type t class type c = object method m : t end end
         end
       In module "K":
       Modules do not match:
         sig type t = K.t class type c = object method m : t/2 end end
       is not included in
         sig type t class type c = object method m : t end end
       In module "K":
       Class type declarations do not match:
         class type c = object method m : t/2 end
       does not match
         class type c = object method m : t end
       The method m has type "t/2" but is expected to have type "t"
       Type "t/2" is not equal to type "t" = "K.t"
       Line 12, characters 4-10:
         Definition of type "t"
       Line 9, characters 2-8:
         Definition of type "t/2"
|}]
;;

module rec M: sig type t type a = M.t end  =
struct type t module M = struct type t end type a = M.t end;;

[%%expect{|
Line 2, characters 0-59:
2 | struct type t module M = struct type t end type a = M.t end;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t module M : sig type t = M.M.t end type a = M.t end
       is not included in
         sig type t type a = M.t end
       Type declarations do not match:
         type a = M.t
       is not included in
         type a = M/2.t
       The type "M.t" = "M/2.M.t" is not equal to the type "M/2.t"
       Line 2, characters 14-42:
         Definition of module "M"
       File "_none_", line 1:
         Definition of module "M/2"
|}]


(** Multiple redefinition of t *)
type t = A;;
type t = B;;
type t = C;;
type t = D;;
module M: sig val f: t -> t -> t -> t end = struct
  let f A B C = D
end;;
[%%expect {|
type t = A
type t = B
type t = C
type t = D
Lines 5-7, characters 44-3:
5 | ............................................struct
6 |   let f A B C = D
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : t/4 -> t/3 -> t/2 -> t end
       is not included in
         sig val f : t -> t -> t -> t end
       Values do not match:
         val f : t/4 -> t/3 -> t/2 -> t
       is not included in
         val f : t -> t -> t -> t
       The type "t/4 -> t/3 -> t/2 -> t" is not compatible with the type
         "t -> t -> t -> t"
       Type "t/4" is not compatible with type "t"
       Line 4, characters 0-10:
         Definition of type "t"
       Line 3, characters 0-10:
         Definition of type "t/2"
       Line 2, characters 0-10:
         Definition of type "t/3"
       Line 1, characters 0-10:
         Definition of type "t/4"
|}]

(** Check interaction with no-alias-deps *)
module Foo = struct
  type info = { doc : unit }
  type t = { info : info }
end
let add_extra_info arg = arg.Foo.info.doc
[%%expect {|
module Foo : sig type info = { doc : unit; } type t = { info : info; } end
Line 5, characters 38-41:
5 | let add_extra_info arg = arg.Foo.info.doc
                                          ^^^
Warning 40 [name-out-of-scope]: "doc" was selected from type "Foo.info".
  It is not visible in the current scope, and will not be selected
  if the type becomes unknown.

val add_extra_info : Foo.t -> unit = <fun>
|}]

(** Check type-directed disambiguation *)
module Bar = struct
  type info = { doc : unit }
end;;
module Foo = struct
  type t = { info : Bar.info }
end;;
module Bar = struct end;;
let add_extra_info arg = arg.Foo.info.doc
[%%expect{|
module Bar : sig type info = { doc : unit; } end
module Foo : sig type t = { info : Bar.info; } end
module Bar : sig end
Line 8, characters 38-41:
8 | let add_extra_info arg = arg.Foo.info.doc
                                          ^^^
Warning 40 [name-out-of-scope]: "doc" was selected from type "Bar/2.info".
  It is not visible in the current scope, and will not be selected
  if the type becomes unknown.

val add_extra_info : Foo.t -> unit = <fun>
|}]
