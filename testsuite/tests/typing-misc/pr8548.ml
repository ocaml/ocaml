(* TEST
   * expect *)

module type Endpoint_intf = sig
  type t
end
;;
[%%expect{|
module type Endpoint_intf = sig type t end
|}]

module type S = sig
  module Endpoint : Endpoint_intf

  type finite = [ `Before of Endpoint.t ]
  type infinite = [ `Until_infinity ]

  type +'a range = private { until : 'a } constraint 'a = [< finite | infinite ]

  val until : 'a range -> 'a
end
;;
[%%expect{|
module type S =
  sig
    module Endpoint : Endpoint_intf
    type finite = [ `Before of Endpoint.t ]
    type infinite = [ `Until_infinity ]
    type +'a range = private { until : 'a; }
      constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
    val until :
      ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
  end
|}]

module type Ranged = sig
  module Endpoint : Endpoint_intf
  module Range : S with type Endpoint.t = Endpoint.t
end
;;
[%%expect{|
module type Ranged =
  sig
    module Endpoint : Endpoint_intf
    module Range :
      sig
        module Endpoint : sig type t = Endpoint.t end
        type finite = [ `Before of Endpoint.t ]
        type infinite = [ `Until_infinity ]
        type +'a range = private { until : 'a; }
          constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
        val until :
          ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
      end
  end
|}]

module Assume (Given : sig
    module Make_range (Endpoint : Endpoint_intf) :
      S with module Endpoint = Endpoint

    module Make_ranged (Range : S) :
      Ranged with module Endpoint = Range.Endpoint
              and module Range = Range
  end) =
struct
  module Point = struct
    type t
  end

  open Given

  module Test_range = Make_range(Point)
  module Test_ranged = Make_ranged(Test_range)
end
;;
[%%expect{|
module Assume :
  functor
    (Given : sig
               module Make_range :
                 functor (Endpoint : Endpoint_intf) ->
                   sig
                     module Endpoint : sig type t = Endpoint.t end
                     type finite = [ `Before of Endpoint.t ]
                     type infinite = [ `Until_infinity ]
                     type +'a range = private { until : 'a; }
                       constraint 'a =
                         [< `Before of Endpoint.t | `Until_infinity ]
                     val until :
                       ([< `Before of Endpoint.t | `Until_infinity ] as 'a)
                       range -> 'a
                   end
               module Make_ranged :
                 functor (Range : S) ->
                   sig
                     module Endpoint : sig type t = Range.Endpoint.t end
                     module Range :
                       sig
                         module Endpoint : sig type t = Range.Endpoint.t end
                         type finite = [ `Before of Endpoint.t ]
                         type infinite = [ `Until_infinity ]
                         type +'a range =
                           'a Range.range = private {
                           until : 'a;
                         }
                           constraint 'a =
                             [< `Before of Endpoint.t | `Until_infinity ]
                         val until :
                           ([< `Before of Endpoint.t | `Until_infinity ]
                            as 'a)
                           range -> 'a
                       end
                   end
             end) ->
    sig
      module Point : sig type t end
      module Test_range :
        sig
          module Endpoint : sig type t = Point.t end
          type finite = [ `Before of Endpoint.t ]
          type infinite = [ `Until_infinity ]
          type +'a range =
            'a Given.Make_range(Point).range = private {
            until : 'a;
          } constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
          val until :
            ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
        end
      module Test_ranged :
        sig
          module Endpoint : sig type t = Test_range.Endpoint.t end
          module Range :
            sig
              module Endpoint : sig type t = Test_range.Endpoint.t end
              type finite = [ `Before of Endpoint.t ]
              type infinite = [ `Until_infinity ]
              type +'a range = 'a Test_range.range = private { until : 'a; }
                constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
              val until :
                ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                'a
            end
        end
    end
|}]
