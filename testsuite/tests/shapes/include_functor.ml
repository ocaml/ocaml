(* TEST
 flags = "-dshape";
 expect;
*)

module type S = sig
  type t
  val x : t
end

module F (X : S) = struct
  type u = X.t
  let y = X.x
end

module M = struct
  type t = T of int
  let x = T 42

  include functor F

  let x = true
  let y = "Not to laugh, not to lament, not to detest, but to understand."
end
[%%expect{|
{
 "S"[module type] -> <.2>;
 }
module type S = sig type t val x : t end
{
 "F"[module] -> Abs<.6>(X, {
                            "u"[type] -> <.4>;
                            "y"[value] -> <.5>;
                            });
 }
module F : (X : S) -> sig type u = X.t val y : X.t end
{
 "M"[module] ->
   {<.12>
    "t"[type] -> {<.7>
                  "T"[constructor] -> {<.8>};
                  };
    "u"[type] -> <.4>;
    "x"[value] -> <.10>;
    "y"[value] -> <.11>;
    };
 }
module M : sig type t = T of int type u = t val x : bool val y : string end
|}]
