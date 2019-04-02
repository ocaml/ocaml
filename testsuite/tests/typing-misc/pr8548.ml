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
Line 1:
Error: Module type declarations do not match:
         module type Ranged =
           sig
             module Endpoint : Endpoint_intf
             module Range :
               sig
                 module Endpoint : sig type t = Endpoint.t end
                 type finite = [ `Before of Endpoint.t ]
                 type infinite = [ `Until_infinity ]
                 type +'a range = private { until : 'a; }
                   constraint 'a =
                     [< `Before of Endpoint.t | `Until_infinity ]
                 val until :
                   ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                   'a
               end
           end
       does not match
         module type Ranged =
           sig
             module Endpoint : Endpoint_intf
             module Range :
               sig
                 module Endpoint : sig type t = Endpoint.t end
                 type finite = [ `Before of Endpoint.t ]
                 type infinite = [ `Until_infinity ]
                 type +'a range = private { until : 'a; }
                   constraint 'a =
                     [< `Before of Endpoint.t | `Until_infinity ]
                 val until :
                   ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                   'a
               end
           end
       At position module type Ranged = <here>
       Modules do not match:
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
                 ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                 'a
             end
         end
       is not included in
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
                 ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range ->
                 'a
             end
         end
       At position module type Ranged = sig module Range : <here> end
       Modules do not match:
         sig
           module Endpoint = Range.Endpoint
           type finite = [ `Before of Endpoint.t ]
           type infinite = [ `Until_infinity ]
           type +'a range = 'a Range.range = private { until : 'a; }
             constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
           val until :
             ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
         end
       is not included in
         sig
           module Endpoint : sig type t = Endpoint.t end
           type finite = [ `Before of Endpoint.t ]
           type infinite = [ `Until_infinity ]
           type +'a range = private { until : 'a; }
             constraint 'a = [< `Before of Endpoint.t | `Until_infinity ]
           val until :
             ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
         end
       At position module type Ranged = sig module Range : <here> end
       Values do not match:
         val until :
           ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
       is not included in
         val until :
           ([< `Before of Endpoint.t | `Until_infinity ] as 'a) range -> 'a
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
Line 6, characters 6-12:
6 |       Ranged with module Endpoint = Range.Endpoint
          ^^^^^^
Error: Unbound module type Ranged
|}]
