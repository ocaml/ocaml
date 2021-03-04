(* TEST
   flags = " -w A -strict-sequence "
   * expect
*)

module Unused : sig
end = struct
  type unused = int
end
;;
[%%expect {|
Line 3, characters 2-19:
3 |   type unused = int
      ^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type unused.
module Unused : sig end
|}]

module Unused_nonrec : sig
end = struct
  type nonrec used = int
  type nonrec unused = used
end
;;
[%%expect {|
Line 4, characters 2-27:
4 |   type nonrec unused = used
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type unused.
module Unused_nonrec : sig end
|}]

module Unused_rec : sig
end = struct
  type unused = A of unused
end
;;
[%%expect {|
Line 3, characters 2-27:
3 |   type unused = A of unused
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type unused.
Line 3, characters 16-27:
3 |   type unused = A of unused
                    ^^^^^^^^^^^
Warning 37 [unused-constructor]: unused constructor A.
module Unused_rec : sig end
|}]

module Used_constructor : sig
  type t
  val t : t
end = struct
  type t = T
  let t = T
end
;;
[%%expect {|
module Used_constructor : sig type t val t : t end
|}]

module Unused_constructor : sig
  type t
end = struct
  type t = T
end
;;
[%%expect {|
Line 4, characters 11-12:
4 |   type t = T
               ^
Warning 37 [unused-constructor]: unused constructor T.
module Unused_constructor : sig type t end
|}]

module Unused_constructor_outside_patterns : sig
  type t
  val nothing : t -> unit
end = struct
  type t = T
  let nothing = function
    | T -> ()
end
;;
[%%expect {|
Line 5, characters 11-12:
5 |   type t = T
               ^
Warning 37 [unused-constructor]: constructor T is never used to build values.
(However, this constructor appears in patterns.)
module Unused_constructor_outside_patterns :
  sig type t val nothing : t -> unit end
|}]

module Unused_constructor_exported_private : sig
  type t = private T
end = struct
  type t = T
end
;;
[%%expect {|
Line 4, characters 11-12:
4 |   type t = T
               ^
Warning 37 [unused-constructor]: constructor T is never used to build values.
Its type is exported as a private type.
module Unused_constructor_exported_private : sig type t = private T end
|}]

module Used_private_constructor : sig
  type t
  val nothing : t -> unit
end = struct
  type t = private T
  let nothing = function
    | T -> ()
end
;;
[%%expect {|
module Used_private_constructor : sig type t val nothing : t -> unit end
|}]

module Unused_private_constructor : sig
  type t
end = struct
  type t = private T
end
;;
[%%expect {|
Line 4, characters 19-20:
4 |   type t = private T
                       ^
Warning 37 [unused-constructor]: unused constructor T.
module Unused_private_constructor : sig type t end
|}]

module Exported_private_constructor : sig
  type t = private T
end = struct
  type t = private T
end
;;
[%%expect {|
module Exported_private_constructor : sig type t = private T end
|}]

module Used_exception : sig
  val e : exn
end = struct
  exception Somebody_uses_me
  let e = Somebody_uses_me
end
;;
[%%expect {|
module Used_exception : sig val e : exn end
|}]

module Used_extension_constructor : sig
  type t
  val t : t
end = struct
  type t = ..
  type t += Somebody_uses_me
  let t = Somebody_uses_me
end
;;
[%%expect {|
module Used_extension_constructor : sig type t val t : t end
|}]

module Unused_exception : sig
end = struct
  exception Nobody_uses_me
end
;;
[%%expect {|
Line 3, characters 2-26:
3 |   exception Nobody_uses_me
      ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 38 [unused-extension]: unused exception Nobody_uses_me
module Unused_exception : sig end
|}]

module Unused_extension_constructor : sig
  type t = ..
end = struct
  type t = ..
  type t += Nobody_uses_me
end
;;
[%%expect {|
Line 5, characters 12-26:
5 |   type t += Nobody_uses_me
                ^^^^^^^^^^^^^^
Warning 38 [unused-extension]: unused extension constructor Nobody_uses_me
module Unused_extension_constructor : sig type t = .. end
|}]

module Unused_exception_outside_patterns : sig
  val falsity : exn -> bool
end = struct
  exception Nobody_constructs_me
  let falsity = function
    | Nobody_constructs_me -> true
    | _ -> false
end
;;
[%%expect {|
Line 4, characters 2-32:
4 |   exception Nobody_constructs_me
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 38 [unused-extension]: exception Nobody_constructs_me is never used to build values.
(However, this constructor appears in patterns.)
module Unused_exception_outside_patterns : sig val falsity : exn -> bool end
|}]

module Unused_extension_outside_patterns : sig
  type t = ..
  val falsity : t -> bool
end = struct
  type t = ..
  type t += Noone_builds_me
  let falsity = function
    | Noone_builds_me -> true
    | _ -> false
end
;;
[%%expect {|
Line 6, characters 12-27:
6 |   type t += Noone_builds_me
                ^^^^^^^^^^^^^^^
Warning 38 [unused-extension]: extension constructor Noone_builds_me is never used to build values.
(However, this constructor appears in patterns.)
module Unused_extension_outside_patterns :
  sig type t = .. val falsity : t -> bool end
|}]

module Unused_exception_exported_private : sig
  type exn += private Private_exn
end = struct
  exception Private_exn
end
;;
[%%expect {|
Line 4, characters 2-23:
4 |   exception Private_exn
      ^^^^^^^^^^^^^^^^^^^^^
Warning 38 [unused-extension]: exception Private_exn is never used to build values.
It is exported or rebound as a private extension.
module Unused_exception_exported_private :
  sig type exn += private Private_exn end
|}]

module Unused_extension_exported_private : sig
  type t = ..
  type t += private Private_ext
end = struct
  type t = ..
  type t += Private_ext
end
;;
[%%expect {|
Line 6, characters 12-23:
6 |   type t += Private_ext
                ^^^^^^^^^^^
Warning 38 [unused-extension]: extension constructor Private_ext is never used to build values.
It is exported or rebound as a private extension.
module Unused_extension_exported_private :
  sig type t = .. type t += private Private_ext end
|}]

module Used_private_extension : sig
  type t
  val nothing : t -> unit
end = struct
  type t = ..
  type t += private Private_ext
  let nothing = function
    | Private_ext | _ -> ()
end
;;
[%%expect {|
module Used_private_extension : sig type t val nothing : t -> unit end
|}]

module Unused_private_extension : sig
  type t
end = struct
  type t = ..
  type t += private Private_ext
end
;;
[%%expect {|
Line 5, characters 20-31:
5 |   type t += private Private_ext
                        ^^^^^^^^^^^
Warning 38 [unused-extension]: unused extension constructor Private_ext
module Unused_private_extension : sig type t end
|}]

module Exported_private_extension : sig
  type t = ..
  type t += private Private_ext
end = struct
  type t = ..
  type t += private Private_ext
end
;;
[%%expect {|
module Exported_private_extension :
  sig type t = .. type t += private Private_ext end
|}]


module Pr7438 : sig
end = struct
  module type S = sig type t = private [> `Foo] end
  module type X =
    sig type t = private [> `Foo | `Bar] include S with type t := t end
end;;
[%%expect {|
module Pr7438 : sig end
|}]

module Unused_type_disable_warning : sig
end = struct
  type t = A [@@warning "-34"]
end;;
[%%expect {|
Line 3, characters 11-12:
3 |   type t = A [@@warning "-34"]
               ^
Warning 37 [unused-constructor]: unused constructor A.
module Unused_type_disable_warning : sig end
|}]

module Unused_constructor_disable_warning : sig
end = struct
  type t = A [@@warning "-37"]
end;;
[%%expect {|
Line 3, characters 2-30:
3 |   type t = A [@@warning "-37"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.
module Unused_constructor_disable_warning : sig end
|}]
