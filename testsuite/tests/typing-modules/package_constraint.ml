(* TEST
 expect;
*)

(* You may constrain abstract types in packages. *)
module type S = sig
  type t
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
|}];;

(* You may use variables in the current environment in the new definitions. *)
module type S = sig
  type t
end

type 'a m = (module S with type t = 'a);;
[%%expect{|
module type S = sig type t end
type 'a m = (module S with type t = 'a)
|}];;

(* It works with non-trivial paths. *)
module type S = sig
  module M : sig
    type t
  end
end

type m = (module S with type M.t = int)
[%%expect{|
module type S = sig module M : sig type t end end
type m = (module S with type M.t = int)
|}];;

(* It should respect immediacy - [m1] should typecheck but not [m2]. *)
(* This is fixed in subsequent commit. *)
module type S = sig
  type t [@@immediate]
end

type m1 = (module S with type t = int)
type m2 = (module S with type t = string);;
[%%expect{|
module type S = sig type t [@@immediate] end
Line 5, characters 10-38:
5 | type m1 = (module S with type t = int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
|}];;

(* You may not constrain types with a manifest in a package *)
module type S = sig
  type t = int
end

type m = (module S with type t = string);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-40:
5 | type m = (module S with type t = string);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match: type t is not included in type t = int
       The type "t/2" is not equal to the type "int"
       Line 2, characters 2-14:
         Definition of type "t"
       Line 5, characters 9-40:
         Definition of type "t/2"
|}];;

(* Even if your constraint would be satisfied. *)
(* It would be nice if this worked. *)
module type S = sig
  type t = int
end

type m = (module S with type t = int);;
[%%expect{|
module type S = sig type t = int end
Line 5, characters 9-37:
5 | type m = (module S with type t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match: type t is not included in type t = int
       The type "t/2" is not equal to the type "int"
       Line 2, characters 2-14:
         Definition of type "t"
       Line 5, characters 9-37:
         Definition of type "t/2"
|}];;

(* And even if the manifest is not obvious in the original definition. *)
module M = struct
  type t
end

module type S = sig
  module P = M
end

type m = (module S with type P.t = int);;
[%%expect{|
module M : sig type t end
module type S = sig module P = M end
Line 9, characters 9-39:
9 | type m = (module S with type P.t = int);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "P.t"
       does not match its original definition in the constrained signature:
       Type declarations do not match: type t is not included in type t = M.t
       The type "t/2" is not equal to the type "M.t"
       Line 2, characters 2-8:
         Definition of type "t"
       Line 9, characters 9-39:
         Definition of type "t/2"
|}];;

(* If writing a package constraint in a mutually recursive group of type decls,
   checking that the constraint's immediacy may not rely on the definitions of
   other elements of the mutually recursive group. *)
(* It would be nice if this worked. *)
module type S = sig
  type t [@@immediate]
end

type t1 = int
and t2 = (module S with type t = t1);;
[%%expect{|
module type S = sig type t [@@immediate] end
Line 6, characters 9-36:
6 | and t2 = (module S with type t = t1);;
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t [@@immediate]
       The first is not an immediate type.
|}];;

(* When using a package with constraint to give an abstract type a definition
   that is immediate, that immediacy information should be usable after
   unpacking. *)
(* This is fixed in a subsequent commit. *)
module type S = sig
  type t
end

type m = (module S with type t = int)

module F (X : sig val x : m end) = struct
  module M = (val X.x)
  type t = M.t [@@immediate]
end;;
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
Line 9, characters 2-28:
9 |   type t = M.t [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Types marked with the immediate attribute must be non-pointer types
       like "int" or "bool".
|}];;
