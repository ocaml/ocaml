module type Endpoint_intf = sig
  type t
end

module type S = sig
  module Endpoint : Endpoint_intf

  type finite = [ `Before of Endpoint.t ]
  type infinite = [ `Until_infinity ]

  type +'a range = private { until : 'a } constraint 'a = [< finite | infinite ]

  val compare_range : ('a -> 'a -> int) -> 'a range -> 'a range -> int

  type t = [finite | infinite] range
end
