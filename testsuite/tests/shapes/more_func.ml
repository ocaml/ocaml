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
 "F"[module] -> Abs<.2>(X, {<.0>});
 }
module F : (X : sig end) -> sig end
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
 "F"[module] -> Abs<.7>(X, {
                            "t"[type] -> <.6>;
                            });
 }
module F : (X : sig end) -> sig type t end
{
 "App"[module] -> {<.8>
                   "t"[type] -> <.6>;
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
 "F"[module] -> Abs<.11>(X, X<.10>);
 }
module F : (X : sig end) -> sig end
{
 "App"[module] -> {<.12>};
 }
module App : sig end
|}]

module Id(X : sig end) = X
module Struct = struct
  module L = List
end
[%%expect{|
{
 "Id"[module] -> Abs<.14>(X, X<.13>);
 }
module Id : (X : sig end) -> sig end
{
 "Struct"[module] ->
   {<.16>
    "L"[module] -> Alias(<.15>
                         CU Stdlib . "List"[module]);
    };
 }
module Struct : sig module L = List end
|}]

module App = Id(List) (* this should have the App uid *)
module Proj = Struct.L
  (* this should have the Proj uid and be an alias to Struct.L *)
[%%expect{|
{
 "App"[module] -> (CU Stdlib . "List"[module])<.17>;
 }
module App : sig end
{
 "Proj"[module] -> Alias(<.18>
                         Alias(<.15>
                               CU Stdlib . "List"[module]));
 }
module Proj = Struct.L
|}]

module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
[%%expect{|
{
 "F"[module] -> Abs<.21>(X, {
                             "M"[module] -> X<.19>;
                             });
 }
module F : (X : sig end) -> sig module M : sig end end
{
 "N"[module] -> {<.22>
                 "M"[module] -> {<.19>};
                 };
 }
module N : sig module M : sig end end
{
 "O"[module] -> Alias(<.23>
                      {<.19>});
 }
module O = N.M
|}]
