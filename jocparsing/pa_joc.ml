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
let nullproc = Grammar.Entry.create gram "nullproc"
let topjpatt = Grammar.Entry.create gram "topjpatt"
let ijpatt = Grammar.Entry.create gram "ijpatt"
let label_ijpatt = Grammar.Entry.create gram "label_ijpatt"
let ijpatt_label_ident = Grammar.Entry.create gram "label_ijpatt"

EXTEND (* join calculus *)
 joinident:
   [[id=LIDENT -> (loc, id)]];

 ijpatt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> PaAcc (loc,p1,p2) ]
    | RIGHTA
      [ i = UIDENT -> PaUid (loc,i)
      | i = LIDENT -> PaLid (loc,i) ] ]
  ;

 label_ijpatt:
    [ [ i = ijpatt_label_ident; "="; p = ijpatt -> (i, p) ] ];

 ijpatt:
     [[
       "{"; lpl = LIST1 label_ijpatt SEP ";"; "}" -> PaRec (loc, lpl)
      | "("; ")" -> PaUid (loc, "()")
      | "("; p = SELF; ")" -> p
      | "("; p = SELF; "as"; p2 = SELF; ")" -> PaAli (loc, p, p2)
      | "("; p = SELF; ","; pl = LIST1 SELF SEP ","; ")" ->
          PaTup (loc, p::pl)
      | s = LIDENT -> PaLid (loc, s)
      | "_" -> PaAny loc
      ]];

 topjpatt:
     [[
       "{"; lpl = LIST1 label_ijpatt SEP ";"; "}" -> PaRec (loc, lpl)
      | "("; ")" -> PaUid (loc, "()")
      | "("; p = ijpatt ; ")" -> p
      | "("; p = ijpatt ; "as"; p2 = SELF; ")" -> PaAli (loc, p, p2)
      | "("; p = ijpatt ; ","; pl = LIST1 ijpatt SEP ","; ")" ->
          PaTup (loc, p::pl)
      ]];

 joinpattern:
   [[id=joinident ; pat=topjpatt -> (loc, id, pat)]];

 joinclause:
   [[
    jpats = LIST1 joinpattern SEP "&" ; "=" ; e=nullproc -> (loc, jpats, e)
  | jpats = LIST1 joinpattern SEP "&" ; "=" ; e=expr -> (loc, jpats, e)
   ]];
   
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

 expr: LEVEL "simple"
    [[
       "{" ; "}" ->  ExNul (loc)
    ]];

 expr: BEFORE "expr1" 
    ["top" RIGHTA [
      e1 = SELF ; ";"; e2 = SELF ->  ExSeq (loc,e1::get_seq e2)
      | e1 = SELF; ";" -> e1
    ]];

 expr: AFTER "top"
    [RIGHTA[
       e1 = SELF; "&"; e2 = SELF -> ExPar (loc, e1, e2)
    ]];

 expr: LEVEL "expr1"
    [[
        "reply" ; "to" ; id = joinident -> ExRep (loc, ExUid (loc, "()"), id)
     |  "reply" ; e = SELF ; "to" ; id = joinident -> ExRep (loc, e, id)
     |  "spawn" ;  e = bracedproc -> ExSpa (loc, e)
     |  "exec" ; "{" ;  e = expr LEVEL "top" ; "}" -> ExSpa (loc, e)
     |  "let" ; "def" ; d = LIST1 joinautomaton SEP "and" ;
        "in" ; e=expr LEVEL "top" ->
        ExDef (loc, d, e)
     | "let" ; "loc" ; d = LIST1 joinlocation SEP "and" ;
        "in" ; e=expr LEVEL "top" ->
        ExLoc (loc, d, e)
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

  nullproc:
    [[
       "{" ; "}" ->  ExNul (loc)
    ]];
END



EXTEND (* dynamic typing *)
 expr: LEVEL "apply"
    [[
       "dynamic"; "module"; e = module_expr -> ExDtm (loc, e)
     | "dynamic"; e = expr -> ExDyn (loc, e)
     | "coerce"; "("; e = SELF; ":"; t=ctyp; ")" -> ExDco (loc, e, t)
    ]];

 module_expr:
    [[
      "coerce"; e = expr; ":"; s = module_type -> MeDtm (loc, e, s)
    ]];
END
