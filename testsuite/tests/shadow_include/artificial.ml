(* TEST
   * expect
   flags = "-nostdlib -nopervasives"
*)

module Foo : sig
  type t

  module Bar : sig
    type t
  end

  val to_ : t -> Bar.t
  val from: Bar.t -> t
end = struct
  type t

  module Bar = struct
    type nonrec t = t
  end

  let to_ x = x
  let from x = x
end
;;
[%%expect{|
module Foo :
  sig
    type t
    module Bar : sig type t end
    val to_ : t -> Bar.t
    val from : Bar.t -> t
  end
|}]

module Extended = struct
  include Foo
  module Bar = struct
    include Bar
    let int = 42
  end
end
;;
[%%expect{|
module Extended :
  sig
    type t = Foo.t
    val to_ : t -> Foo.Bar.t
    val from : Foo.Bar.t -> t
    module Bar : sig type t = Foo.Bar.t val int : int end
  end
|}]

module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
    val int : int
  end
end
;;
[%%expect{|
module type Extended =
  sig
    type t = Foo.t
    val to_ : t -> Foo.Bar.t
    val from : Foo.Bar.t -> t
    module Bar : sig type t = Foo.Bar.t val int : int end
  end
|}]
