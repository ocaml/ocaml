(* TEST
*)

[@@@ocaml.warning "@A"]
[@@@ocaml.alert "++all"]

(* Fixture *)

module type DEPRECATED = sig end
[@@ocaml.deprecated]

module T = struct
  type deprecated
  [@@ocaml.deprecated]
end

(* Structure items *)

let _ = let x = 1 in ()
[@@ocaml.warning "-26"]

include (struct let _ = let x = 1 in () end)
[@@ocaml.warning "-26"]

module A = struct let _ = let x = 1 in () end
[@@ocaml.warning "-26"]

module rec B : sig type t end = struct type t = T.deprecated end
[@@ocaml.alert "-deprecated"]

module type T = sig type t = T.deprecated end
[@@ocaml.alert "-deprecated"]

(* Signature items *)

module type S = sig
  val x : T.deprecated
  [@@ocaml.alert "-deprecated"]

  module AA : sig type t = T.deprecated end
  [@@ocaml.alert "-deprecated"]

  module rec BB : sig type t = T.deprecated end
  [@@ocaml.alert "-deprecated"]

  module type T = sig type t = T.deprecated end
  [@@ocaml.alert "-deprecated"]

  include DEPRECATED
  [@@ocaml.alert "-deprecated"]
end
