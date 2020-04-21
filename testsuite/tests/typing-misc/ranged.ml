module Make (Range : Range_intf.S) :
  Ranged_intf.S with module Endpoint = Range.Endpoint
                 and module Range = Range
= struct
  module Endpoint = Range.Endpoint
  module Range = Range
end

module Test = struct
  type t = T
end

module Test_range = Range.Make(Test)
module Test_ranged = Make(Test_range)
