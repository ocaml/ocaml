(* TEST
  * expect
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
Line _, characters 8-52:
  ........struct
      type t = B
      let f B = ()
    end
Error: Signature mismatch:
       Modules do not match:
         sig type t = B val f : t -> unit end
       is not included in
         sig val f : t -> unit end
       Values do not match:
         val f : t/1 -> unit
       is not included in
         val f : t/2 -> unit
       Line _, characters 4-14:
      type t = B
      ^^^^^^^^^^
Definition of type t/1
Line _, characters 2-12:
    type t = A
    ^^^^^^^^^^
Definition of type t/2
|}]

module N = struct
  type t= A
  module M: sig type u = A of t end =
  struct type t = B type u = A of t end
end;;
[%%expect{|
Line _, characters 2-39:
    struct type t = B type u = A of t end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = B type u = A of t end
       is not included in
         sig type u = A of t end
       Type declarations do not match:
         type u = A of t/1
       is not included in
         type u = A of t/2
       The types for field A are not equal.
       Line _, characters 9-19:
    struct type t = B type u = A of t end
           ^^^^^^^^^^
Definition of type t/1
Line _, characters 2-11:
    type t= A
    ^^^^^^^^^
Definition of type t/2
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
Line _, characters 4-70:
  ....struct
        module type s
        module A(X:s) =struct end
      end
Error: Signature mismatch:
       Modules do not match:
         sig module type s module A : functor (X : s) -> sig  end end
       is not included in
         sig module A : functor (X : s) -> sig  end end
       In module A:
       Modules do not match:
         functor (X : s/1) -> sig  end
       is not included in
         functor (X : s/2) -> sig  end
       At position module A(X : <here>) : ...
       Modules do not match: s/2 is not included in s/1
       Line _, characters 6-19:
        module type s
        ^^^^^^^^^^^^^
Definition of module type s/1
Line _, characters 2-15:
    module type s
    ^^^^^^^^^^^^^
Definition of module type s/2
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
Line _, characters 4-77:
  ....struct
        module T = struct type t end
        type t = A of T.t
      end
Error: Signature mismatch:
       Modules do not match:
         sig module T : sig type t end type t = A of T.t end
       is not included in
         sig type t = A of T.t end
       Type declarations do not match:
         type t = A of T/1.t
       is not included in
         type t = A of T/2.t
       The types for field A are not equal.
       Line _, characters 6-34:
        module T = struct type t end
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of module T/1
Line _, characters 2-30:
    module T = struct type t end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of module T/2
|}]

module O = struct
  module type s
  type t = A
  module M: sig val f: (module s) -> t -> t end =
  struct module type s type t = B let f (module X:s) A = B end
end;;

[%%expect{|
Line _, characters 2-62:
    struct module type s type t = B let f (module X:s) A = B end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig module type s type t = B val f : (module s) -> t/2 -> t/1 end
       is not included in
         sig val f : (module s) -> t -> t end
       Values do not match:
         val f : (module s/1) -> t/2 -> t/1
       is not included in
         val f : (module s/2) -> t/2 -> t/2
       Line _, characters 23-33:
    struct module type s type t = B let f (module X:s) A = B end
                         ^^^^^^^^^^
Definition of type t/1
Line _, characters 2-12:
    type t = A
    ^^^^^^^^^^
Definition of type t/2
Line _, characters 9-22:
    struct module type s type t = B let f (module X:s) A = B end
           ^^^^^^^^^^^^^
Definition of module type s/1
Line _, characters 2-15:
    module type s
    ^^^^^^^^^^^^^
Definition of module type s/2
|}]

module P = struct
  module type a
  type a = A
   module M : sig val f: a -> (module a) -> a  end
   = struct type a = B let f A _  = B end
end;;

[%%expect{|
Line _, characters 5-41:
     = struct type a = B let f A _  = B end
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type a = B val f : a/2 -> 'a -> a/1 end
       is not included in
         sig val f : a -> (module a) -> a end
       Values do not match:
         val f : a/2 -> 'a -> a/1
       is not included in
         val f : a/2 -> (module a) -> a/2
       Line _, characters 12-22:
     = struct type a = B let f A _  = B end
              ^^^^^^^^^^
Definition of type a/1
Line _, characters 2-12:
    type a = A
    ^^^^^^^^^^
Definition of type a/2
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
Line _, characters 2-105:
  ..struct
      class a = object method c = let module X = struct type t end in () end
      class b = a
    end
Error: Signature mismatch:
       Modules do not match:
         sig class a : object method c : unit end class b : a end
       is not included in
         sig class b : a end
       Class declarations do not match:
         class b : a
       does not match
         class b : a/2
       The first class type has no method m
       The public method c cannot be hidden
       Line _, characters 4-74:
      class a = object method c = let module X = struct type t end in () end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of class type a/1
Line _, characters 2-36:
    class a = object method m = () end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of class type a/2
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
Line _, characters 2-65:
  ..struct
      class type a = object end
      class type b = a
    end
Error: Signature mismatch:
       Modules do not match:
         sig class type a = object  end class type b = a end
       is not included in
         sig class type b = a end
       Class type declarations do not match:
         class type b = a/1
       does not match
         class type b = a/2
       The first class type has no method m
       Line _, characters 4-29:
      class type a = object end
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of class type a/1
Line _, characters 2-42:
    class type a = object method m: unit end
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of class type a/2
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
Line _, characters 6-141:
  ......struct
    type t
    class type a = object method m:t end
    module K = struct
      type t
      class type c = object inherit a end
    end
  end..
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
       In module K:
       Modules do not match:
         sig type t = K.t class type c = object method m : t/2 end end
       is not included in
         sig type t class type c = object method m : t end end
       In module K:
       Class type declarations do not match:
         class type c = object method m : t/2 end
       does not match
         class type c = object method m : t/1 end
       The method m has type t/2 but is expected to have type t/1
       Type t/2 is not compatible with type t/1 = K.t
       Line _, characters 4-10:
      type t
      ^^^^^^
Definition of type t/1
Line _, characters 2-8:
    type t
    ^^^^^^
Definition of type t/2
|}]
;;

module rec M: sig type t type a = M.t end  =
struct type t module M = struct type t end type a = M.t end;;

[%%expect{|
Line _, characters 0-59:
  struct type t module M = struct type t end type a = M.t end;;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = M.t module M : sig type t = M.M.t end type a = M.t end
       is not included in
         sig type t type a = M.t end
       Type declarations do not match:
         type a = M/1.t
       is not included in
         type a = M/2.t
       Line _, characters 14-42:
  struct type t module M = struct type t end type a = M.t end;;
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Definition of module M/1
Line _:
Definition of module M/2
|}]
