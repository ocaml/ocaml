open Camlp4.PreCast;
value _loc = Loc.ghost;

module Term = struct
  type patt =
    [ PApp of patt and patt
    | PAny
    | PVar of string
    | POlb of string and expr ]
  and expr =
    [ EApp of expr and expr
    | EVar of string
    | ELam of patt and expr ];
end;

module MetaTerm = MetaGenerator Term;
