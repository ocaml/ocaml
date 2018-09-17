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
    type unused = int
    ^^^^^^^^^^^^^^^^^
Warning 34: unused type unused.
module Unused : sig  end
|}]

module Unused_nonrec : sig
end = struct
  type nonrec used = int
  type nonrec unused = used
end
;;
[%%expect {|
Line 4, characters 2-27:
    type nonrec unused = used
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34: unused type unused.
module Unused_nonrec : sig  end
|}]

module Unused_rec : sig
end = struct
  type unused = A of unused
end
;;
[%%expect {|
Line 3, characters 2-27:
    type unused = A of unused
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34: unused type unused.
Line 3, characters 2-27:
    type unused = A of unused
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 37: unused constructor A.
module Unused_rec : sig  end
|}]

module Unused_exception : sig
end = struct
  exception Nobody_uses_me
end
;;
[%%expect {|
Line 3, characters 2-26:
    exception Nobody_uses_me
    ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 38: unused exception Nobody_uses_me
module Unused_exception : sig  end
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
    type t += Nobody_uses_me
              ^^^^^^^^^^^^^^
Warning 38: unused extension constructor Nobody_uses_me
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
    exception Nobody_constructs_me
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 38:
exception Nobody_constructs_me is never used to build values.
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
    type t += Noone_builds_me
              ^^^^^^^^^^^^^^^
Warning 38:
extension constructor Noone_builds_me is never used to build values.
(However, this constructor appears in patterns.)
module Unused_extension_outside_patterns :
  sig type t = .. val falsity : t -> bool end
|}]

module Unused_private_exception : sig
  type exn += private Private_exn
end = struct
  exception Private_exn
end
;;
[%%expect {|
Line 4, characters 2-23:
    exception Private_exn
    ^^^^^^^^^^^^^^^^^^^^^
Warning 38:
exception Private_exn is never used to build values.
It is exported or rebound as a private extension.
module Unused_private_exception : sig type exn += private Private_exn end
|}]

module Unused_private_extension : sig
  type t = ..
  type t += private Private_ext
end = struct
  type t = ..
  type t += Private_ext
end
;;
[%%expect {|
Line 6, characters 12-23:
    type t += Private_ext
              ^^^^^^^^^^^
Warning 38:
extension constructor Private_ext is never used to build values.
It is exported or rebound as a private extension.
module Unused_private_extension :
  sig type t = .. type t += private Private_ext end
|}]

module Pr7438 : sig
end = struct
  module type S = sig type t = private [> `Foo] end
  module type X =
    sig type t = private [> `Foo | `Bar] include S with type t := t end
end;;
[%%expect {|
module Pr7438 : sig  end
|}]

module Unused_type_disable_warning : sig
end = struct
  type t = A [@@warning "-34"]
end;;
[%%expect {|
Line 3, characters 2-30:
    type t = A [@@warning "-34"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 37: unused constructor A.
module Unused_type_disable_warning : sig  end
|}]

module Unused_constructor_disable_warning : sig
end = struct
  type t = A [@@warning "-37"]
end;;
[%%expect {|
Line 3, characters 2-30:
    type t = A [@@warning "-37"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34: unused type t.
module Unused_constructor_disable_warning : sig  end
|}]
