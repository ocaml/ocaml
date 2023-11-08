(* TEST
 flags = "-dshape";
 expect;
*)

module A = struct type t end
module B = A
[%%expect{|
{
 "A"[module] -> {<.1>
                 "t"[type] -> {<.0>};
                 };
 }
module A : sig type t end
{
 "B"[module] -> Alias(<.2>
                      {<.1>
                       "t"[type] -> {<.0>};
                       });
 }
module B = A
|}]

type u = B.t

[%%expect{|
{
 "u"[type] -> {<.3>};
 }
type u = B.t
|}]

module F (X : sig type t end) = X
module F' = F
[%%expect{|
{
 "F"[module] -> Abs<.6>(X/282, X/282<.5>);
 }
module F : functor (X : sig type t end) -> sig type t = X.t end
{
 "F'"[module] -> Alias(<.7>
                       Abs<.6>(X/282, X/282<.5>));
 }
module F' = F
|}]

module C = F'(A)
[%%expect{|
{
 "C"[module] -> {<.8>
                 "t"[type] -> {<.0>};
                 };
 }
module C : sig type t = A.t end
|}]


module C = F(B)

[%%expect{|
{
 "C"[module] -> Alias(<.9>
                      {<.1>
                       "t"[type] -> {<.0>};
                       });
 }
module C : sig type t = B.t end
|}]

module D = C

[%%expect{|
{
 "D"[module] -> Alias(<.10>
                      Alias(<.9>
                            {<.1>
                             "t"[type] -> {<.0>};
                             }));
 }
module D = C
|}]

module G (X : sig type t end) = struct include X end
[%%expect{|
{
 "G"[module] -> Abs<.13>(X/294, {
                                 "t"[type] -> X/294<.12> . "t"[type];
                                 });
 }
module G : functor (X : sig type t end) -> sig type t = X.t end
|}]

module E = G(B)
[%%expect{|
{
 "E"[module] -> {<.14>
                 "t"[type] -> {<.0>};
                 };
 }
module E : sig type t = B.t end
|}]
