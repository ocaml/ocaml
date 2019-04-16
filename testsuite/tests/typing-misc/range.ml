module Make (Endpoint : Range_intf.Endpoint_intf) :
  Range_intf.S with module Endpoint = Endpoint
= struct
  module Endpoint = Endpoint

  type finite = [ `Before of Endpoint.t ]
  type infinite = [ `Until_infinity ]

  type +'a range = { until : 'a } constraint 'a = [< finite | infinite ]

  let until r = r.until

  type t = [finite | infinite] range

  let compare_range _ _ _ = 0
end
