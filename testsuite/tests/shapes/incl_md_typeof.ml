(* TEST
   flags = "-dshape"
   * expect
*)

module Foo : sig
  module Bar : sig
  end
end = struct
  module Bar = struct
  end
end
;;
[%%expect{|
{
 "Foo"[module] -> {<.2>
                   "Bar"[module] -> {<.0>
                                     };
                   };
 }
module Foo : sig module Bar : sig end end
|}]

module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
  end
end
;;
[%%expect{|
{
 "Extended"[module type] -> <.4>;
 }
module type Extended = sig module Bar : sig end end
|}]

module E : Extended = struct
  module Bar = struct end
end

[%%expect{|
{
 "E"[module] -> {<.6>
                 "Bar"[module] -> {<.5>
                                   };
                 };
 }
module E : Extended
|}]
