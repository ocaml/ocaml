open Stdpp;;
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
let joinpattern = Grammar.Entry.create gram "joinpattern"
let joinclause = Grammar.Entry.create gram "joinclause"
let joinautomaton = Grammar.Entry.create gram "joinautomaton"
let joinlocation =  Grammar.Entry.create gram "joinlocation"
let bracedproc = Grammar.Entry.create gram "bracedproc"
let ipatt = Grammar.Entry.create gram "ipatt"

EXTEND
 joinident:
   [[id=LIDENT -> (loc, id)]];

 label_ijpatt:
    [ [ i = patt_label_ident; "="; p = ijpatt -> (i, p) ] ];

 ijpatt:
     [ [ "{"; lpl = LIST1 label_ijpatt SEP ";"; "}" -> PaRec loc lpl
      | "("; ")" ->
          PaCon
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 ipatt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | s = LIDENT -> <:patt< $lid:s$ >>
      | "_" -> <:patt< _ >> ] ]

 joinpattern:
   [[id=joinident ; pat=ijpatt ->
     (loc, id, pat)]];

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

