(* TEST
   * expect
*)

module M = struct
  type t = int
  let x = 42
end
;;
[%%expect{|
module M : sig type t = int val x : int end
|}]
class c =
  let open M in
  object
    method f : t = x
  end
;;
[%%expect{|
class c : object method f : M.t end
|}]
class type ct =
  let open M in
  object
    method f : t
  end
;;
[%%expect{|
class type ct = object method f : M.t end
|}]
