(* TEST
   flags = "-dshape"
   * expect
*)

module type S = sig
  module M: sig
    (** A module M *)
  end

  module type T = module type of struct include M end
end

[%%expect{|
{
 "S"[module type] -> <.2>;
 }
module type S = sig module M : sig end module type T = sig end end
|}]
