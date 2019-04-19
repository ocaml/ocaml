module type S = sig
  module Endpoint : Range_intf.Endpoint_intf
  module Range : Range_intf.S with type Endpoint.t = Endpoint.t
end
