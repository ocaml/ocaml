(* TEST
 expect;
*)

module Unstable_api : sig
  [@@@unstable_feature "unstable_type"]

  type t [@@unstable { feature = "unstable_type"; issue = 1111 }]
  type never_enabled [@@unstable { feature = "dont_enable"; issue = 6666 }]

  val make : unit -> t [@@unstable { feature = "unstable_val"; issue = 2222 }]
  val stable : unit -> int

  val never_enabled : unit -> int
    [@@unstable { feature = "dont_enable"; issue = 6666 }]

  type constr =
    | Stable
    | Unstable [@unstable { feature = "unstable_constr"; issue = 3333 }]
    | Never_enabled [@unstable { feature = "dont_enable"; issue = 6666 }]

  type record =
    { stable : int
    ; unstable : bool [@unstable { feature = "unstable_label"; issue = 4444 }]
    ; never_enabled : unit [@unstable { feature = "dont_enable"; issue = 6666 }]
    }

  val make_record : unit -> record
end = struct
  type t = unit
  type never_enabled = unit

  let make () = ()
  let stable () = 0
  let never_enabled () = 1

  type constr =
    | Stable
    | Unstable
    | Never_enabled

  type record =
    { stable : int
    ; unstable : bool
    ; never_enabled : unit
    }

  let make_record () = { stable = 42; unstable = false; never_enabled = () }
end

(* Types *)

type disabled_unstable_t = Unstable_api.t

[%%expect
{|
module Unstable_api :
  sig
    type t
    type never_enabled
    val make : unit -> t
    val stable : unit -> int
    val never_enabled : unit -> int
    type constr = Stable | Unstable | Never_enabled
    type record = { stable : int; unstable : bool; never_enabled : unit; }
    val make_record : unit -> record
  end
Line 49, characters 27-41:
49 | type disabled_unstable_t = Unstable_api.t
                                ^^^^^^^^^^^^^^
Error: Unstable_api.t uses unstable feature 'unstable_type' (issue #1111).
       Enable it with -Z unstable_type or [@@@unstable_feature "unstable_type"]
|}]

[@@@unstable_feature "unstable_type"]

type unstable_t = Unstable_api.t

[%%expect {|
type unstable_t = Unstable_api.t
|}]

(* Unstable values are accessible from stable ones *)

let should_fail_unstable_val = Unstable_api.make

[%%expect
{|
Line 1, characters 31-48:
1 | let should_fail_unstable_val = Unstable_api.make
                                   ^^^^^^^^^^^^^^^^^
Error: Unstable_api.make uses unstable feature 'unstable_val' (issue #2222).
       Enable it with -Z unstable_val or [@@@unstable_feature "unstable_val"]
|}]

[@@@unstable_feature "unstable_val"]

let should_work_unstable_val = Unstable_api.make

[%%expect {|
val should_work_unstable_val : unit -> Unstable_api.t = <fun>
|}]

module Inner = struct
  let unstable_make = Unstable_api.make
end

[%%expect
{|
module Inner : sig val unstable_make : unit -> Unstable_api.t end
|}]

module Outer = struct
  module Inner = struct
    let unstable_make = Unstable_api.make
  end
end

[%%expect
{|
module Outer :
  sig module Inner : sig val unstable_make : unit -> Unstable_api.t end end
|}]

(* Constructors and records *)

let r = Unstable_api.make_record ()
let should_fail_unstable_constr = Unstable_api.Unstable
let should_fail_unstable_record = r.Unstable_api.unstable_label

[%%expect
{|
val r : Unstable_api.record =
  {Unstable_api.stable = 42; unstable = false; never_enabled = ()}
Line 2, characters 34-55:
2 | let should_fail_unstable_constr = Unstable_api.Unstable
                                      ^^^^^^^^^^^^^^^^^^^^^
Error: Unstable uses unstable feature 'unstable_constr' (issue #3333).
       Enable it with -Z unstable_constr or [@@@unstable_feature "unstable_constr"]
Unexecuted phrases: 1 phrases did not execute due to an error
|}]

[@@@unstable_feature "unstable_constr,unstable_label"]

let should_work_unstable_constr = Unstable_api.Unstable
let should_work_unstable_record = r.Unstable_api.unstable

[%%expect
{|
val should_work_unstable_constr : Unstable_api.constr = Unstable_api.Unstable
val should_work_unstable_record : bool = false
|}]

(* Nested scopes *)

module New_scope = struct
  [@@@unstable_feature "different_feature"]

  module Nested_unstable : sig
    val nested : string
      [@@unstable { feature = "different_feature"; issue = 7777 }]
  end = struct
    let nested = "nested"
  end

  let a = Nested_unstable.nested
end

let should_fail = New_scope.Nested_unstable.nested

[%%expect
{|
module New_scope :
  sig module Nested_unstable : sig val nested : string end val a : string end
Line 14, characters 18-50:
14 | let should_fail = New_scope.Nested_unstable.nested
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: New_scope.Nested_unstable.nested uses unstable feature 'different_feature' (issue #7777).
       Enable it with -Z different_feature or [@@@unstable_feature "different_feature"]
|}]

let still_works = New_scope.a

[%%expect {|
val still_works : string = "nested"
|}]
