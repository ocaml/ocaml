(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_
syntax__Filler_text_added_to_pr
*)

exception Foo [@foo] [@@foo]

exception Bar of (int [@foo]) [@foo] [@@foo]

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

  exception Bar of (int [@foo]) [@foo] [@@foo]

  include (module type of (M[@foo]))[@foo] with type t := M.t[@foo]
    [@@foo]

  [@@@foo]

end[@foo]
[@@foo]

[@@@foo]

module type T = sig type t end

module _ = (Int : T [@foo])

module _ = (Int : T with type t = int [@foo])

let _ = (module Int : T [@foo])

let _ = (module Int : T with type t = int [@foo])

(* TEST
 flags = "-dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
