open Pcaml;;
open MLast;;


DELETE_RULE
 expr: SELF ; "&" ; SELF
END

DELETE_RULE
 expr: SELF ; "or" ; SELF
END

(* Big pm needed to get location, compiled code is optimal! *)
let get_loc = function
  | ExAcc (l, _, _)
  | ExAnt (l, _)
  | ExApp (l, _, _)
  | ExAre (l, _, _)
  | ExArr (l, _)
  | ExAss (l, _, _)
  | ExChr (l, _)
  | ExCoe (l, _, _, _)
  | ExFlo (l, _)
  | ExFor (l, _, _, _, _, _)
  | ExFun (l, _)
  | ExIfe (l, _, _, _)
  | ExInt (l, _)
  | ExLab (l, _, _)
  | ExLet (l, _, _, _)
  | ExLid (l, _)
  | ExLmd (l, _, _, _)
  | ExMat (l, _, _)
  | ExNew (l, _)
  | ExOlb (l, _, _)
  | ExOvr (l, _)
  | ExRec (l, _, _)
  | ExSeq (l, _)
  | ExSnd (l, _, _)
  | ExSte (l, _, _)
  | ExStr (l, _)
  | ExTry (l, _, _)
  | ExTup (l, _)
  | ExTyc (l, _, _)
  | ExUid (l, _)
  | ExVrn (l, _)
  | ExWhi (l, _, _)
  | ExLoc (l, _, _)
  | ExDef (l, _, _)
  | ExRep (l, _, _)
  | ExNul l
  | ExPar (l, _, _)
  | ExSpa (l, _) -> l


(* Some kind of append, needed because "&" is left-associative *)
let rec add_par l e1 e2 = match e1 with
| ExPar (m, f1, f2) ->
    let (start_f2, _) = get_loc f2
    and (_, end_l) = l in
    ExPar (l, f1, add_par (start_f2, end_l) f2 e2)
| _ -> ExPar (l, e1, e2)

let joinident = Grammar.Entry.create gram "joinident"
let joinpattern = Grammar.Entry.create gram "joinpattern"
let joinclause = Grammar.Entry.create gram "joinclause"
let joinautomaton = Grammar.Entry.create gram "joinautomaton"
let joinlocation =  Grammar.Entry.create gram "joinlocation"

EXTEND
 joinident:
   [[id=LIDENT -> (loc, id)]];

 joinpattern:
   [[id=joinident ; "(" ; args = LIST0 joinident SEP "," ; ")" ->
     (loc, id, args)]];
 joinclause:
   [[jpats = LIST1 joinpattern SEP "&" ; "=" ; e=expr ->
     (loc, jpats, e)]];

 joinautomaton:
   [[auto = LIST1 joinclause SEP "or" ->
     (loc, auto)]];

 joinlocation:
   [[id = joinident ; "def" ; autos = LIST1 joinautomaton SEP "and" ;
    "do" ; e=expr LEVEL "top" ->
      (loc, id, autos, e)
    | id = joinident ;  "do" ; e=expr LEVEL "top" ->
      (loc, id, [], e)
    ]];

 expr: LEVEL "top" 
    [[
      e1 = SELF ; "&"; e2 = SELF -> add_par loc e1 e2
    ]];
 expr: LEVEL "expr1"
    [[
      "reply" ; e = SELF ; "to" ; id = joinident -> ExRep (loc, e, id)
     | "spawn" ; e = SELF -> ExSpa (loc, e)
     | "let" ; "def" ; d = LIST1 joinautomaton SEP "and" ;
        "in" ; e=expr LEVEL "top" ->
        ExDef (loc, d, e)
     | "let" ; "loc" ; d = LIST1 joinlocation SEP "and" ;
        "in" ; e=expr LEVEL "top" ->
        ExLoc (loc, d, e)
    ]];
 expr: LEVEL "simple"
    [[
      "{" ; "}" -> ExNul (loc)
    ]];
 str_item: LEVEL "top"
    [[
      "let" ; "def" ; d = LIST1 joinautomaton SEP "and" ;
      "in" ; e=expr ->
        StExp (loc, ExDef (loc, d, e))
    | "let" ; "loc" ; d = LIST1 joinlocation SEP "and" ;
       "in" ; e=expr ->
        StExp (loc, ExLoc (loc, d, e))

    | "let" ; "def" ; d = LIST1 joinautomaton SEP "and" ->
         StDef (loc, d)
    | "let" ; "loc" ; d = LIST1 joinlocation SEP "and"  ->
        StLoc (loc, d)
    ]];
END

