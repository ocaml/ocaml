(* TEST
   * expect
   flags = "-nopervasives" (* can't pass -nostdlib because of objects. *)
*)

(* Signatures *)

(* Tests that everything can be shadowed. *)

module type S = sig
  type t

  val unit : unit

  external e : unit -> unit = "%identity"

  module M : sig type t end

  module type T

  exception E

  type ext = ..
  type ext += C

  class c : object end

  class type ct = object end
end
;;
[%%expect{|
module type S =
  sig
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module type SS = sig
  include S
  include S
end
;;
[%%expect{|
module type SS =
  sig
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

(* Test that the call to nondep works properly. *)

module type Type = sig
  include S
  type u = t
  include S
end
;;
[%%expect{|
module type Type =
  sig
    type u
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module type Type_fail = sig
  include S
  val ignore : t -> unit
  include S
end
;;
[%%expect{|
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Illegal shadowing of included type t/131 by t/146
       Line 2, characters 2-11:
         Type t/131 came from this include
       Line 3, characters 2-24:
         The value ignore has no valid type if t/131 is shadowed
|}]

module type Module = sig
  include S
  module N = M
  include S
end
;;
[%%expect{|
module type Module =
  sig
    module N : sig type t end
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module type Module_fail = sig
  include S
  val ignore : M.t -> unit
  include S
end
;;
[%%expect{|
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Illegal shadowing of included module M/211 by M/226
       Line 2, characters 2-11:
         Module M/211 came from this include
       Line 3, characters 2-26:
         The value ignore has no valid type if M/211 is shadowed
|}]


module type Module_type = sig
  include S
  module type U = T
  include S
end
;;
[%%expect{|
module type Module_type =
  sig
    module type U
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module type Module_type_fail = sig
  include S
  module F : functor (_ : T) -> sig end
  include S
end
;;
[%%expect{|
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Illegal shadowing of included module type T/287 by T/302
       Line 2, characters 2-11:
         Module type T/287 came from this include
       Line 3, characters 2-39:
         The module F has no valid type if T/287 is shadowed
|}]

module type Extension = sig
  include S
  type ext += C2
  include S
end
;;
[%%expect{|
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Illegal shadowing of included type ext/318 by ext/333
       Line 2, characters 2-11:
         Type ext/318 came from this include
       Line 3, characters 14-16:
         The extension constructor C2 has no valid type if ext/318 is shadowed
|}]

module type Class = sig
  include S
  class parametrized : int -> c
  include S
end
;;
[%%expect{|
module type Class =
  sig
    class parametrized : int -> object  end
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module type Class_type = sig
  include S
  class type parametrized = ct
  include S
end
;;
[%%expect{|
module type Class_type =
  sig
    class type parametrized = object  end
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig type t end
    module type T
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

(* Structures *)

(* Tests that everything can be shadowed. *)

module N = struct
  type t

  let unit = ()

  external e : unit -> unit = "%identity"

  module M = struct end

  module type T = sig end

  exception E

  type ext = ..
  type ext += C

  class c = object end

  class type ct = object end
end
;;
[%%expect{|
module N :
  sig
    type t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M : sig end
    module type T = sig end
    exception E
    type ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module NN = struct
  include N
  include N
end
;;
[%%expect{|
module NN :
  sig
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

(* Test that the call to nondep works properly *)

module Type = struct
  include N
  type u = t
  include N
end
;;
[%%expect{|
module Type :
  sig
    type u = N.t
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Module = struct
  include N
  module O = M
  include N
end
;;
[%%expect{|
module Module :
  sig
    module O = N.M
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Module_type = struct
  include N
  module type U = T
  include N
end
;;
[%%expect{|
module Module_type :
  sig
    module type U = N.T
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Exception = struct
  include N
  exception Exn = E
  include N
end
;;
[%%expect{|
module Exception :
  sig
    exception Exn
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Extension = struct
  include N
  type ext += C2
  include N
end
;;
[%%expect{|
module Extension :
  sig
    type N.ext += C2
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Class = struct
  include N
  class parametrized _ = c
  include N
end
;;
[%%expect{|
module Class :
  sig
    class parametrized : 'a -> object  end
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]

module Class_type = struct
  include N
  class type parametrized = ct
  include N
end
;;
[%%expect{|
module Class_type :
  sig
    class type parametrized = object  end
    type t = N.t
    val unit : unit
    external e : unit -> unit = "%identity"
    module M = N.M
    module type T = N.T
    exception E
    type ext = N.ext = ..
    type ext += C
    class c : object  end
    class type ct = object  end
  end
|}]
