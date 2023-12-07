(* TEST
 flags = "-dshape";
 expect;
*)

module M = struct end (* uid 0 *)
module F(X : sig end) = M
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.0>};
 }
module M : sig end
{
 "F"[module] -> Abs<.2>(X/278, {<.0>});
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.3>};
 }
module App : sig end
|}]


module M = struct end (* uid 4 *)
module F(X : sig end) = struct include M type t end
module App = F(List)
[%%expect{|
{
 "M"[module] -> {<.4>};
 }
module M : sig end
{
 "F"[module] -> Abs<.7>(X/352, {
                                "t"[type] -> {<.6>};
                                });
 }
module F : functor (X : sig end) -> sig type t end
{
 "App"[module] -> {<.8>
                   "t"[type] -> {<.6>};
                   };
 }
module App : sig type t = F(List).t end
|}]

module M = struct end (* uid 9 *)
module F(X : sig end) = X
module App = F(M)
[%%expect{|
{
 "M"[module] -> {<.9>};
 }
module M : sig end
{
 "F"[module] -> Abs<.11>(X/366, X/366<.10>);
 }
module F : functor (X : sig end) -> sig end
{
 "App"[module] -> {<.12>};
 }
module App : sig end
|}]
