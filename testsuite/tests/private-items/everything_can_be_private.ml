(* TEST
   * expect
*)

module type Sig = sig
  private val v : int
  private external e : int -> unit = "%identity"
  private type t = int
  private type ext = ..
  private type ext += Private_constr
  private exception Exn
  private class c : object end
  private class type ct = object end
end
;;
[%%expect{|
module type Sig = sig  end
|}]

module Struct = struct
  private let v = 3
  private external e : int -> unit = "%identity"
  private type t = int
  private type ext = ..
  private type ext += Private_constr
  private exception Exn
  private class c = object end
  private class type ct = object end
end
;;
[%%expect{|
module Struct : sig  end
|}]
