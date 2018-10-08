(* TEST
   * expect
*)

type bar = < bar: unit >

type _ ty = Int : int ty

type dyn = Dyn : 'a ty -> dyn;;

class foo =
  object (this)
    method foo (Dyn ty) =
      match ty with
      | Int -> (this :> bar)
  end;;  (* fail, but not for scope *)

[%%expect{|
type bar = < bar : unit >
type _ ty = Int : int ty
type dyn = Dyn : 'a ty -> dyn
Line 7, characters 0-108:
 7 | class foo =
   |   object (this)
   |     method foo (Dyn ty) =
   |       match ty with
   |       | Int -> (this :> bar)
12 |   end.................................
Error: This class should be virtual.
       The following methods are undefined : bar
|}];;
