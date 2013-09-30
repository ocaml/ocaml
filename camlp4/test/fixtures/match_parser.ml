open Camlp4.PreCast;

let _loc = Loc.ghost in
let e = <:expr< parser [: `"a" :] -> t >> in
let a =
  match e with
  [ <:expr< parser [: `$str:x$ :] -> t >> -> x
  | _ -> assert False ]
in Format.printf "a: %S@." a;
