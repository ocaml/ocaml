open Pcaml;;
open MLast;;


DELETE_RULE
 expr: SELF ; "&" ; SELF
END

DELETE_RULE
 expr: SELF ; "or" ; SELF
END


DELETE_RULE
 expr: SELF ; ";"  ; SELF
END


DELETE_RULE
 expr: SELF ; ";"
END


let get_seq = function
  | ExSeq (loc, el) -> el
  | e               -> [e]


let joinident = Grammar.Entry.create gram "joinident"
let joinarg   = Grammar.Entry.create gram "joinarg"
let joinpattern = Grammar.Entry.create gram "joinpattern"
let joinclause = Grammar.Entry.create gram "joinclause"
let joinautomaton = Grammar.Entry.create gram "joinautomaton"
let joinlocation =  Grammar.Entry.create gram "joinlocation"
let bracedproc = Grammar.Entry.create gram "bracedproc"
EXTEND
 joinident:
   [[id=LIDENT -> (loc, id)]];

 joinarg:
   [[
     id=LIDENT -> (loc, Some id)
  |  "_"       -> (loc, None)
   ]];

 joinpattern:
   [[id=joinident ; "(" ; args = LIST0 joinarg SEP "," ; ")" ->
     (loc, id, args)]];
 joinclause:
   [[jpats = LIST1 joinpattern SEP "&" ; "=" ; e=expr ->
     (loc, jpats, e)]];

 joinautomaton:
   [[auto = LIST1 joinclause SEP "or" ->
     (loc, auto)]];

 joinlocation:
   [[id = joinident ;  "def" ; autos = LIST1 joinautomaton SEP "and" ;
    "do" ; e=bracedproc ->
      (loc, id, autos, e)
    | id = joinident ;  "do" ; e = bracedproc ->
       (loc, id, [], e)
    ]];

 expr: BEFORE "expr1" 
    ["top" RIGHTA [
      e1 = SELF ; "&"; e2 = SELF -> ExPar (loc, e1, e2)
    ]];
 expr: AFTER "top"
    [[
       e1 = SELF; ";"; e2 = SELF ->  ExSeq (loc,e1::get_seq e2)
      | e1 = SELF; ";" -> e1
    ]];

 expr: LEVEL "expr1"
    [[
        "reply" ; "to" ; id = joinident -> ExRep (loc, ExUid (loc, "()"), id)
     |  "reply" ; e = SELF ; "to" ; id = joinident -> ExRep (loc, e, id)
     | "spawn" ;  e = bracedproc -> ExSpa (loc, e)
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
  bracedproc:
    [[
      "{" ; "}" ->  ExNul (loc)
  |  "{" ; e=expr LEVEL "top" ; "}" -> e
    ]];
END

