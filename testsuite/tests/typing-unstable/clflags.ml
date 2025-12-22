(* TEST
 flags = "-Z unstable_type,unstable_val,unstable_constr,unstable_label";
 expect;
*)

module Unstable_api : sig
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

let x = Unstable_api.make ()

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
val x : Unstable_api.t = <abstr>
|}]

let y = Unstable_api.stable ()

[%%expect {|
val y : int = 0
|}]

let z = Unstable_api.never_enabled ()

[%%expect
{|
Line 1, characters 8-34:
1 | let z = Unstable_api.never_enabled ()
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unstable_api.never_enabled uses unstable feature 'dont_enable' (issue #6666).
       Enable it with -Z dont_enable or [@@@unstable_feature "dont_enable"]
|}]

type unstable_t = Unstable_api.t

[%%expect {|
type unstable_t = Unstable_api.t
|}]

type never_enabled_t = Unstable_api.never_enabled

[%%expect
{|
Line 1, characters 23-49:
1 | type never_enabled_t = Unstable_api.never_enabled
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unstable_api.never_enabled uses unstable feature 'dont_enable' (issue #6666).
       Enable it with -Z dont_enable or [@@@unstable_feature "dont_enable"]
|}]

let a = Unstable_api.Stable

[%%expect {|
val a : Unstable_api.constr = Unstable_api.Stable
|}]

let b = Unstable_api.Unstable

[%%expect {|
val b : Unstable_api.constr = Unstable_api.Unstable
|}]

let c = Unstable_api.Never_enabled

[%%expect
{|
Line 1, characters 8-34:
1 | let c = Unstable_api.Never_enabled
            ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Never_enabled uses unstable feature 'dont_enable' (issue #6666).
       Enable it with -Z dont_enable or [@@@unstable_feature "dont_enable"]
|}]

let r = Unstable_api.make_record ()

[%%expect
{|
val r : Unstable_api.record =
  {Unstable_api.stable = 42; unstable = false; never_enabled = ()}
|}]

let field = r.Unstable_api.stable

[%%expect {|
val field : int = 42
|}]

let field2 = r.Unstable_api.unstable

[%%expect {|
val field2 : bool = false
|}]

let field3 = r.Unstable_api.never_enabled

[%%expect
{|
Line 1, characters 15-41:
1 | let field3 = r.Unstable_api.never_enabled
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: never_enabled uses unstable feature 'dont_enable' (issue #6666).
       Enable it with -Z dont_enable or [@@@unstable_feature "dont_enable"]
|}]
