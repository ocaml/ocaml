(* TEST
 flags = " -rectypes ";
 ocamlrunparam += "l=1000000";
 expect;
*)

(* PR14730 tests #11648 *)

module Empty = struct end

module Empty_wrapper = struct
  module Empty = Empty
end

open Empty_wrapper

module Make (_ : sig end) = struct
  type (_, 't) open_t = A : 't -> (int, 't) open_t
end

open Make (Empty)

type int_t = (int, int_t) open_t

type 'a t = ('a, int_t) open_t
type any = Any : 'a t -> any

let hang (f : any -> bool) (v : any) : bool =
  match v with
  | Any A a -> f (Any a)
[%%expect{|
module Empty : sig end
module Empty_wrapper : sig module Empty = Empty end
module Make :
  sig end -> sig type (_, 't) open_t = A : 't -> (int, 't) open_t end
type ('a, 't) open_t =
  ('a, 't) Make(Empty_wrapper.Empty).open_t =
    A : 't -> (int, 't) open_t
type int_t = (int, int_t) open_t
type 'a t = ('a, int_t) open_t
type any = Any : 'a t -> any
val hang : (any -> bool) -> any -> bool = <fun>
|}]
