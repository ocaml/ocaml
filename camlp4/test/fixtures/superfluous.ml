open Camlp4.PreCast;;
open Syntax;;

let _loc = Loc.ghost;;
let st = <:str_item< >>;;
let e = <:expr< 1 >>
let bi = <:binding< x = 0 >>;;

(* none of these holds due to superfluous StSem and StNil *)
assert (Ast.StSem (_loc, st, st) = <:str_item< $st$ $st$ >>);;
assert (Ast.StExp (_loc, e) = <:str_item< $exp:e$ >>);;
assert (Ast.StVal (_loc, bi) = <:str_item< let $bi$ >>);;
