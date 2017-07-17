[@@@ocaml.warning "+3"]

module X: sig
  type t [@@ocaml.deprecated]
  val x: int [@@ocaml.deprecated]
end = struct
  type t
  let x = 0
end

type t1 = X.t
let x1 = X.x

type t2 = X.t * X.t [@@ocaml.warning "-3"]
type t3 = (X.t * X.t) [@ocaml.warning "-3"]
type t4 = (X.t [@ocaml.warning "-3"]) * X.t

type t5 = X.t [@@ocaml.warning "-3"]
and t6 = X.t

type t7 = A of t7
    [@@ocaml.deprecated]

type t8 = A of (t8 [@ocaml.warning "-3"])
    [@@ocaml.deprecated]

type t9 = A of t9
  [@@ocaml.deprecated]
  [@@ocaml.warning "-3"]
