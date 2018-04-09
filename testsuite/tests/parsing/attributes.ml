(* TEST
   flags = "-dparsetree"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

[@@@foo]

let (x[@foo]) : unit [@foo] = ()[@foo]
  [@@foo]

type t =
  | Foo of (t[@foo]) [@foo]
[@@foo]

[@@@foo]


module M = struct
  type t = {
    l : (t [@foo]) [@foo]
  }
    [@@foo]
    [@@foo]

  [@@@foo]
end[@foo]
[@@foo]

module type S = sig

  include (module type of (M[@foo]))[@foo] with type t := M.t[@foo]
    [@@foo]

  [@@@foo]

end[@foo]
[@@foo]

[@@@foo]
