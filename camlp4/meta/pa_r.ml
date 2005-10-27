(* camlp4r pa_extend.cmo q_MLast.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Stdpp;
open Pcaml;

Pcaml.no_constructors_arity.val := False;

value help_sequences () =
  do {
    Printf.eprintf "\
New syntax:
     do {e1; e2; ... ; en}
     while e do {e1; e2; ... ; en}
     for v = v1 to/downto v2 do {e1; e2; ... ; en}
Old (discouraged) syntax:
     do e1; e2; ... ; en-1; return en
     while e do e1; e2; ... ; en; done
     for v = v1 to/downto v2 do e1; e2; ... ; en; done
To avoid compilation warning use the new syntax.
";
    flush stderr;
    exit 1
  }
;
Pcaml.add_option "-help_seq" (Arg.Unit help_sequences)
  "Print explanations about new sequences and exit.";

do {
  let odfa = Plexer.dollar_for_antiquotation.val in
  Plexer.dollar_for_antiquotation.val := False;
  let (lexer, pos) = Plexer.make_lexer () in
  Pcaml.position.val := pos;
  Grammar.Unsafe.gram_reinit gram lexer;
  Plexer.dollar_for_antiquotation.val := odfa;
  Grammar.Unsafe.clear_entry interf;
  Grammar.Unsafe.clear_entry implem;
  Grammar.Unsafe.clear_entry top_phrase;
  Grammar.Unsafe.clear_entry use_file;
  Grammar.Unsafe.clear_entry module_type;
  Grammar.Unsafe.clear_entry module_expr;
  Grammar.Unsafe.clear_entry sig_item;
  Grammar.Unsafe.clear_entry str_item;
  Grammar.Unsafe.clear_entry expr;
  Grammar.Unsafe.clear_entry patt;
  Grammar.Unsafe.clear_entry ctyp;
  Grammar.Unsafe.clear_entry let_binding;
  Grammar.Unsafe.clear_entry type_declaration;
  Grammar.Unsafe.clear_entry class_type;
  Grammar.Unsafe.clear_entry class_expr;
  Grammar.Unsafe.clear_entry class_sig_item;
  Grammar.Unsafe.clear_entry class_str_item
};

Pcaml.parse_interf.val := Grammar.Entry.parse interf;
Pcaml.parse_implem.val := Grammar.Entry.parse implem;

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

value mksequence _loc =
  fun
  [ [e] -> e
  | el -> <:expr< do { $list:el$ } >> ]
;

value mkmatchcase _loc p aso w e =
  let p =
    match aso with
    [ Some p2 -> <:patt< ($p$ as $p2$) >>
    | _ -> p ]
  in
  (p, w, e)
;
      
value neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n
;

value mkumin _loc f arg =
  match arg with
  [ <:expr< $int:n$ >> -> <:expr< $int:neg_string n$ >>
  | MLast.ExInt32 loc n -> MLast.ExInt32 loc (neg_string n)
  | MLast.ExInt64 loc n -> MLast.ExInt64 loc (neg_string n)
  | MLast.ExNativeInt loc n -> MLast.ExNativeInt loc (neg_string n)
  | <:expr< $flo:n$ >> -> <:expr< $flo:neg_string n$ >>
  | _ ->
      let f = "~" ^ f in
      <:expr< $lid:f$ $arg$ >> ]
;

value mklistexp _loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let _loc =
          if top then _loc else (fst (MLast.loc_of_expr e1), snd _loc)
        in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat _loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let _loc =
          if top then _loc else (fst (MLast.loc_of_patt p1), snd _loc)
        in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value mkexprident _loc ids = match ids with
  [ [] -> Stdpp.raise_with_loc _loc (Stream.Error "illegal long identifier")
  | [ id :: ids ] ->
      let rec loop m = fun
        [ [ id :: ids ] -> loop <:expr< $m$ . $id$ >> ids
        | [] -> m ]
  in
  loop id ids ]
;

value mkassert _loc e =
  match e with
  [ <:expr< False >> -> MLast.ExAsf _loc
  | _ -> MLast.ExAsr _loc e ]
;

value append_elem el e = el @ [e];

(* ...suppose to flush the input in case of syntax error to avoid multiple
   errors in case of cut-and-paste in the xterm, but work bad: for example
   the input "for x = 1;" waits for another line before displaying the
   error...
value rec sync cs =
  match cs with parser
  [ [: `';' :] -> sync_semi cs
  | [: `_ :] -> sync cs ]
and sync_semi cs =
  match Stream.peek cs with 
  [ Some ('\010' | '\013') -> ()
  | _ -> sync cs ]
;
Pcaml.sync.val := sync;
*)

value ipatt = Grammar.Entry.create gram "ipatt";
value with_constr = Grammar.Entry.create gram "with_constr";
value row_field = Grammar.Entry.create gram "row_field";

value not_yet_warned_variant = ref True;
value warn_variant loc =
  if not_yet_warned_variant.val then do {
    not_yet_warned_variant.val := False;
    Pcaml.warning.val loc
      (Printf.sprintf
         "use of syntax of variants types deprecated since version 3.05");
  }
  else ()
;

value not_yet_warned = ref True;
value warn_sequence loc =
  if not_yet_warned.val then do {
    not_yet_warned.val := False;
    Pcaml.warning.val loc
      ("use of syntax of sequences deprecated since version 3.01.1");
  }
  else ()
;
Pcaml.add_option "-no_warn_seq" (Arg.Clear not_yet_warned)
  "No warning when using old syntax for sequences.";

EXTEND
  GLOBAL: sig_item str_item ctyp patt expr module_type module_expr class_type
    class_expr class_sig_item class_str_item let_binding type_declaration
    ipatt with_constr row_field;
  module_expr:
    [ [ "functor"; "("; i = UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
      | "struct"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:module_expr< struct $list:st$ end >> ]
    | [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
    | [ me1 = SELF; "."; me2 = SELF -> <:module_expr< $me1$ . $me2$ >> ]
    | "simple"
      [ i = UIDENT -> <:module_expr< $uid:i$ >>
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "("; me = SELF; ")" -> <:module_expr< $me$ >> ] ]
  ;
  str_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = str_item; ";" -> s ]; "end" ->
          <:str_item< declare $list:st$ end >>
      | "exception"; (_, c, tl) = constructor_declaration; b = rebind_exn ->
          <:str_item< exception $c$ of $list:tl$ = $b$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; me = module_expr -> <:str_item< include $me$ >>
      | "module"; i = UIDENT; mb = module_binding ->
          <:str_item< module $i$ = $mb$ >>
      | "module"; "rec"; nmtmes = LIST1 module_rec_binding SEP "and" ->
          MLast.StRecMod _loc nmtmes
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:str_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:str_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "value"; r = OPT "rec"; l = LIST1 let_binding SEP "and" ->
          <:str_item< value $opt:o2b r$ $list:l$ >>
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  ;
  rebind_exn:
    [ [ "="; sl = mod_ident -> sl
      | -> [] ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
      | ":"; mt = module_type; "="; me = module_expr ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
  ;
  module_rec_binding:
    [ [ m = UIDENT; ":"; mt = module_type; "="; me = module_expr ->
          (m, mt, me) ] ]
  ;
  module_type:
    [ [ "functor"; "("; i = UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
    | [ mt = SELF; "with"; wcl = LIST1 with_constr SEP "and" ->
          <:module_type< $mt$ with $list:wcl$ >> ]
    | [ "sig"; sg = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:module_type< sig $list:sg$ end >> ]
    | [ m1 = SELF; m2 = SELF -> <:module_type< $m1$ $m2$ >> ]
    | [ m1 = SELF; "."; m2 = SELF -> <:module_type< $m1$ . $m2$ >> ]
    | "simple"
      [ i = UIDENT -> <:module_type< $uid:i$ >>
      | i = LIDENT -> <:module_type< $lid:i$ >>
      | "'"; i = ident -> <:module_type< ' $i$ >>
      | "("; mt = SELF; ")" -> <:module_type< $mt$ >> ] ]
  ;
  sig_item:
    [ "top"
      [ "declare"; st = LIST0 [ s = sig_item; ";" -> s ]; "end" ->
          <:sig_item< declare $list:st$ end >>
      | "exception"; (_, c, tl) = constructor_declaration ->
          <:sig_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; mt = module_type -> <:sig_item< include $mt$ >>
      | "module"; i = UIDENT; mt = module_declaration ->
          <:sig_item< module $i$ : $mt$ >>
      | "module"; "rec"; mds = LIST1 module_rec_declaration SEP "and" ->
          MLast.SgRecMod _loc mds
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:sig_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:sig_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "value"; i = LIDENT; ":"; t = ctyp ->
          <:sig_item< value $i$ : $t$ >> ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> <:module_type< $mt$ >>
      | "("; i = UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
  ;
  module_rec_declaration:
    [ [ m = UIDENT; ":"; mt = module_type -> (m, mt)] ]
  ;
  with_constr:
    [ [ "type"; i = mod_ident; tpl = LIST0 type_parameter; "="; t = ctyp ->
          <:with_constr< type $i$ $list:tpl$ = $t$ >>
      | "module"; i = mod_ident; "="; me = module_expr ->
          <:with_constr< module $i$ = $me$ >> ] ]
  ;
  expr:
    [ "top" RIGHTA
      [ "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = SELF ->
          <:expr< let $opt:o2b r$ $list:l$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding; "in"; e = SELF ->
          <:expr< let module $m$ = $mb$ in $e$ >>
      | "fun"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = ipatt; e = fun_def -> <:expr< fun $p$ -> $e$ >>
      | "match"; e = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< match $e$ with [ $list:l$ ] >>
      | "match"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          <:expr< match $e$ with $p1$ -> $e1$ >>
      | "try"; e = SELF; "with"; "["; l = LIST0 match_case SEP "|"; "]" ->
          <:expr< try $e$ with [ $list:l$ ] >>
      | "try"; e = SELF; "with"; p1 = ipatt; "->"; e1 = SELF ->
          <:expr< try $e$ with $p1$ -> $e1$ >>
      | "if"; e1 = SELF; "then"; e2 = SELF; "else"; e3 = SELF ->
          <:expr< if $e1$ then $e2$ else $e3$ >>
      | "do"; "{"; seq = sequence; "}" -> mksequence _loc seq
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; "{"; seq = sequence; "}" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:seq$ } >>
      | "while"; e = SELF; "do"; "{"; seq = sequence; "}" ->
          <:expr< while $e$ do { $list:seq$ } >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          (* <:expr< object $opt:cspo$ $list:cf$ end >> *)
          MLast.ExObj _loc cspo cf ]
    | "where"
      [ e = SELF; "where"; rf = OPT "rec"; lb = let_binding ->
          <:expr< let $opt:o2b rf$ $list:[lb]$ in $e$ >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy -> <:expr< $e1$ := $e2$ >> ]
    | "||" RIGHTA
      [ e1 = SELF; "||"; e2 = SELF -> <:expr< $e1$ || $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; "&&"; e2 = SELF -> <:expr< $e1$ && $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF; "<"; e2 = SELF -> <:expr< $e1$ < $e2$ >>
      | e1 = SELF; ">"; e2 = SELF -> <:expr< $e1$ > $e2$ >>
      | e1 = SELF; "<="; e2 = SELF -> <:expr< $e1$ <= $e2$ >>
      | e1 = SELF; ">="; e2 = SELF -> <:expr< $e1$ >= $e2$ >>
      | e1 = SELF; "="; e2 = SELF -> <:expr< $e1$ = $e2$ >>
      | e1 = SELF; "<>"; e2 = SELF -> <:expr< $e1$ <> $e2$ >>
      | e1 = SELF; "=="; e2 = SELF -> <:expr< $e1$ == $e2$ >>
      | e1 = SELF; "!="; e2 = SELF -> <:expr< $e1$ != $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF; "^"; e2 = SELF -> <:expr< $e1$ ^ $e2$ >>
      | e1 = SELF; "@"; e2 = SELF -> <:expr< $e1$ @ $e2$ >> ]
    | "+" LEFTA
      [ e1 = SELF; "+"; e2 = SELF -> <:expr< $e1$ + $e2$ >>
      | e1 = SELF; "-"; e2 = SELF -> <:expr< $e1$ - $e2$ >>
      | e1 = SELF; "+."; e2 = SELF -> <:expr< $e1$ +. $e2$ >>
      | e1 = SELF; "-."; e2 = SELF -> <:expr< $e1$ -. $e2$ >> ]
    | "*" LEFTA
      [ e1 = SELF; "*"; e2 = SELF -> <:expr< $e1$ * $e2$ >>
      | e1 = SELF; "/"; e2 = SELF -> <:expr< $e1$ / $e2$ >>
      | e1 = SELF; "*."; e2 = SELF -> <:expr< $e1$ *. $e2$ >>
      | e1 = SELF; "/."; e2 = SELF -> <:expr< $e1$ /. $e2$ >>
      | e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
      | e1 = SELF; "lor"; e2 = SELF -> <:expr< $e1$ lor $e2$ >>
      | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
      | e1 = SELF; "mod"; e2 = SELF -> <:expr< $e1$ mod $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF; "**"; e2 = SELF -> <:expr< $e1$ ** $e2$ >>
      | e1 = SELF; "asr"; e2 = SELF -> <:expr< $e1$ asr $e2$ >>
      | e1 = SELF; "lsl"; e2 = SELF -> <:expr< $e1$ lsl $e2$ >>
      | e1 = SELF; "lsr"; e2 = SELF -> <:expr< $e1$ lsr $e2$ >> ]
    | "unary minus" NONA
      [ "-"; e = SELF -> mkumin _loc "-" e
      | "-."; e = SELF -> mkumin _loc "-." e ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> <:expr< $e1$ $e2$ >>
      | "assert"; e = SELF -> mkassert _loc e
      | "lazy"; e = SELF -> <:expr< lazy ($e$) >> ]
    | "." LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >> ]
    | "~-" NONA
      [ "~-"; e = SELF -> <:expr< ~- $e$ >>
      | "~-."; e = SELF -> <:expr< ~-. $e$ >> ]
    | "simple"
      [ s = INT -> <:expr< $int:s$ >>
      | s = INT32 -> MLast.ExInt32 _loc s
      | s = INT64 -> MLast.ExInt64 _loc s
      | s = NATIVEINT -> MLast.ExNativeInt _loc s
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | s = CHAR -> <:expr< $chr:s$ >>
      | ids = expr_ident -> mkexprident _loc ids
      | "["; "]" -> <:expr< [] >>
      | "["; el = LIST1 expr SEP ";"; last = cons_expr_opt; "]" ->
          mklistexp _loc last el
      | "[|"; el = LIST0 expr SEP ";"; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; lel = LIST1 label_expr SEP ";"; "}" -> <:expr< { $list:lel$ } >>
      | "{"; "("; e = SELF; ")"; "with"; lel = LIST1 label_expr SEP ";"; "}"
        -> <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ","; el = LIST1 expr SEP ","; ")" ->
          <:expr< ( $list:[e::el]$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >> ] ]
  ;
  cons_expr_opt:
    [ [ "::"; e = expr -> Some e
      | -> None ] ]
  ;
  dummy:
    [ [ -> () ] ]
  ;
  sequence:
    [ [ "let"; rf = OPT "rec"; l = LIST1 let_binding SEP "and"; [ "in" | ";" ];
        el = SELF ->
          [<:expr< let $opt:o2b rf$ $list:l$ in $mksequence _loc el$ >>]
      | e = expr; ";"; el = SELF -> [e :: el]
      | e = expr; ";" -> [e]
      | e = expr -> [e] ] ]
  ;
  let_binding:
    [ [ p = ipatt; e = fun_binding -> (p, e) ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "="; e = expr -> <:expr< $e$ >>
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >> 
      | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e$ :> $t$) >> ] ]
  ;
  match_case:
    [ [ p = patt; aso = as_patt_opt; w = when_expr_opt; "->"; e = expr ->
          mkmatchcase _loc p aso w e ] ]
  ;
  as_patt_opt:
    [ [ "as"; p = patt -> Some p
      | -> None ] ]
  ;
  when_expr_opt:
    [ [ "when"; e = expr -> Some e
      | -> None ] ]
  ;
  label_expr:
    [ [ i = patt_label_ident; e = fun_binding -> (i, e) ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> [ <:expr< $lid:i$ >> ]
      | i = UIDENT -> [ <:expr< $uid:i$ >> ]
      | i = UIDENT; "."; j = SELF -> [ <:expr< $uid:i$ >> :: j ] ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = ipatt; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = expr -> e ] ]
  ;
  patt:
    [ LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
    | LEFTA
      [ p1 = SELF; p2 = SELF -> <:patt< $p1$ $p2$ >> ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple"
      [ s = LIDENT -> <:patt< $lid:s$ >>
      | s = UIDENT -> <:patt< $uid:s$ >>
      | s = INT -> <:patt< $int:s$ >>
      | s = INT32 -> MLast.PaInt32 _loc s
      | s = INT64 -> MLast.PaInt64 _loc s
      | s = NATIVEINT -> MLast.PaNativeInt _loc s
      | s = FLOAT -> <:patt< $flo:s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | s = CHAR -> <:patt< $chr:s$ >>
      | "-"; s = INT -> MLast.PaInt _loc (neg_string s)
      | "-"; s = INT32 -> MLast.PaInt32 _loc (neg_string s)
      | "-"; s = INT64 -> MLast.PaInt64 _loc (neg_string s)
      | "-"; s = NATIVEINT -> MLast.PaNativeInt _loc (neg_string s)
      | "-"; s = FLOAT -> <:patt< $flo:neg_string s$ >>
      | "["; "]" -> <:patt< [] >>
      | "["; pl = LIST1 patt SEP ";"; last = cons_patt_opt; "]" ->
          mklistpat _loc last pl
      | "[|"; pl = LIST0 patt SEP ";"; "|]" -> <:patt< [| $list:pl$ |] >>
      | "{"; lpl = LIST1 label_patt SEP ";"; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 patt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | "_" -> <:patt< _ >> ] ]
  ;
  cons_patt_opt:
    [ [ "::"; p = patt -> Some p
      | -> None ] ]
  ;
  label_patt:
    [ [ i = patt_label_ident; "="; p = patt -> (i, p) ] ]
  ;
  patt_label_ident:
    [ LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple" RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  ipatt:
    [ [ "{"; lpl = LIST1 label_ipatt SEP ";"; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; "as"; p2 = SELF; ")" -> <:patt< ($p$ as $p2$) >>
      | "("; p = SELF; ","; pl = LIST1 ipatt SEP ","; ")" ->
          <:patt< ( $list:[p::pl]$) >>
      | s = LIDENT -> <:patt< $lid:s$ >>
      | "_" -> <:patt< _ >> ] ]
  ;
  label_ipatt:
    [ [ i = patt_label_ident; "="; p = ipatt -> (i, p) ] ]
  ;
  type_declaration:
    [ [ n = type_patt; tpl = LIST0 type_parameter; "="; tk = ctyp;
        cl = LIST0 constrain ->
          (n, tpl, tk, cl) ] ]
  ;
  type_patt:
    [ [ n = LIDENT -> (_loc, n) ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> (i, (False, False))
      | "+"; "'"; i = ident -> (i, (True, False))
      | "-"; "'"; i = ident -> (i, (False, True)) ] ]
  ;
  ctyp:
    [ LEFTA
      [ t1 = SELF; "=="; t2 = SELF -> <:ctyp< $t1$ == $t2$ >> ]
    | NONA
      [ "private"; t = ctyp LEVEL "alias" -> <:ctyp< private $t$ >> ]
    | "alias" LEFTA
      [ t1 = SELF; "as"; t2 = SELF -> <:ctyp< $t1$ as $t2$ >> ]
    | LEFTA
      [ "!"; pl = LIST1 typevar; "."; t = ctyp ->
          <:ctyp< ! $list:pl$ . $t$ >> ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
    | "label" NONA
      [ i = TILDEIDENT; ":"; t = SELF -> <:ctyp< ~ $i$ : $t$ >>
      | i = LABEL; t =  SELF  -> <:ctyp< ~ $i$ : $t$ >>
      | i = QUESTIONIDENT; ":"; t = SELF -> <:ctyp< ? $i$ : $t$ >>
      | i = OPTLABEL; t = SELF -> <:ctyp< ? $i$ : $t$ >> ]
    | LEFTA
      [ t1 = SELF; t2 = SELF -> <:ctyp< $t1$ $t2$ >> ]
    | LEFTA
      [ t1 = SELF; "."; t2 = SELF -> <:ctyp< $t1$ . $t2$ >> ]
    | "simple"
      [ "'"; i = ident -> <:ctyp< '$i$ >>
      | "_" -> <:ctyp< _ >>
      | i = LIDENT -> <:ctyp< $lid:i$ >>
      | i = UIDENT -> <:ctyp< $uid:i$ >>
      | "("; t = SELF; "*"; tl = LIST1 ctyp SEP "*"; ")" ->
          <:ctyp< ( $list:[t::tl]$ ) >>
      | "("; t = SELF; ")" -> <:ctyp< $t$ >>
      | "["; cdl = LIST0 constructor_declaration SEP "|"; "]" ->
          <:ctyp< [ $list:cdl$ ] >>
          (* MLast.TySum _loc cdl *)
      | "{"; ldl = LIST1 label_declaration SEP ";"; "}" ->
          <:ctyp< { $list:ldl$ } >>
          (* MLast.TyRec _loc ldl *) ] ]
  ;
  constructor_declaration:
    [ [ ci = UIDENT; "of"; cal = LIST1 ctyp SEP "and" -> (_loc, ci, cal)
      | ci = UIDENT -> (_loc, ci, []) ] ]
  ;
  label_declaration:
    [ [ i = LIDENT; ":"; mf = OPT "mutable"; t = ctyp ->
          (_loc, i, o2b mf, t) ] ]
  ;
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> [i]
      | i = LIDENT -> [i]
      | i = UIDENT; "."; j = SELF -> [i :: j] ] ]
  ;
  (* Objects and Classes *)
  str_item:
    [ [ "class"; cd = LIST1 class_declaration SEP "and" ->
          <:str_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:str_item< class type $list:ctd$ >> ] ]
  ;
  sig_item:
    [ [ "class"; cd = LIST1 class_description SEP "and" ->
          <:sig_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:sig_item< class type $list:ctd$ >> ] ]
  ;
  class_declaration:
    [ [ vf = OPT "virtual"; i = LIDENT; ctp = class_type_parameters;
        cfb = class_fun_binding ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = i; MLast.ciExp = cfb} ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          <:class_expr< ($ce$ : $ct$) >>
      | p = ipatt; cfb = SELF -> <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
  class_type_parameters:
    [ [ -> (_loc, [])
      | "["; tpl = LIST1 type_parameter SEP ","; "]" -> (_loc, tpl) ] ]
  ;
  class_fun_def:
    [ [ p = ipatt; ce = SELF -> <:class_expr< fun $p$ -> $ce$ >>
      | "->"; ce = class_expr -> ce ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; p = ipatt; ce = class_fun_def ->
          <:class_expr< fun $p$ -> $ce$ >>
      | "let"; rf = OPT "rec"; lb = LIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          <:class_expr< let $opt:o2b rf$ $list:lb$ in $ce$ >> ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "label" ->
          <:class_expr< $ce$ $e$ >> ]
    | "simple"
      [ ci = class_longident; "["; ctcl = LIST0 ctyp SEP ","; "]" ->
          <:class_expr< $list:ci$ [ $list:ctcl$ ] >>
      | ci = class_longident -> <:class_expr< $list:ci$ >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          <:class_expr< object $opt:cspo$ $list:cf$ end >>
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          <:class_expr< ($ce$ : $ct$) >>
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = LIST0 [ cf = class_str_item; ";" -> cf ] -> cf ] ]
  ;
  class_self_patt:
    [ [ "("; p = patt; ")" -> p
      | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >> ] ]
  ;
  class_str_item:
    [ [ "declare"; st = LIST0 [ s= class_str_item; ";" -> s ]; "end" ->
          <:class_str_item< declare $list:st$ end >>
      | "inherit"; ce = class_expr; pb = OPT as_lident ->
          <:class_str_item< inherit $ce$ $opt:pb$ >>
      | "value"; mf = OPT "mutable"; lab = label; e = cvalue_binding ->
          <:class_str_item< value $opt:o2b mf$ $lab$ = $e$ >>
      | "method"; "virtual"; pf = OPT "private"; l = label; ":"; t = ctyp ->
          <:class_str_item< method virtual $opt:o2b pf$ $l$ : $t$ >>
      | "method"; pf = OPT "private"; l = label; topt = OPT polyt;
        e = fun_binding ->
          <:class_str_item< method $opt:o2b pf$ $l$ $opt:topt$ = $e$ >>
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_str_item< type $t1$ = $t2$ >>
      | "initializer"; se = expr -> <:class_str_item< initializer $se$ >> ] ]
  ;
  as_lident:
    [ [ "as"; i = LIDENT -> i ] ]
  ;
  polyt:
    [ [ ":"; t = ctyp -> t ] ]
  ;
  cvalue_binding:
    [ [ "="; e = expr -> e
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >>
      | ":"; t = ctyp; ":>"; t2 = ctyp; "="; e = expr ->
          <:expr< ($e$ : $t$ :> $t2$) >>
      | ":>"; t = ctyp; "="; e = expr -> <:expr< ($e$ :> $t$) >> ] ]
  ;
  label:
    [ [ i = LIDENT -> i ] ]
  ;
  class_type:
    [ [ "["; t = ctyp; "]"; "->"; ct = SELF ->
          <:class_type< [ $t$ ] -> $ct$ >>
      | id = clty_longident; "["; tl = LIST1 ctyp SEP ","; "]" ->
          <:class_type< $list:id$ [ $list:tl$ ] >>
      | id = clty_longident -> <:class_type< $list:id$ >>
      | "object"; cst = OPT class_self_type;
        csf = LIST0 [ csf = class_sig_item; ";" -> csf ]; "end" ->
          <:class_type< object $opt:cst$ $list:csf$ end >> ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "declare"; st = LIST0 [ s = class_sig_item; ";" -> s ]; "end" ->
          <:class_sig_item< declare $list:st$ end >>
      | "inherit"; cs = class_type -> <:class_sig_item< inherit $cs$ >>
      | "value"; mf = OPT "mutable"; l = label; ":"; t = ctyp ->
          <:class_sig_item< value $opt:o2b mf$ $l$ : $t$ >>
      | "method"; "virtual"; pf = OPT "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method virtual $opt:o2b pf$ $l$ : $t$ >>
      | "method"; pf = OPT "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method $opt:o2b pf$ $l$ : $t$ >>
      | "type"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_sig_item< type $t1$ = $t2$ >> ] ]
  ;
  class_description:
    [ [ vf = OPT "virtual"; n = LIDENT; ctp = class_type_parameters; ":";
        ct = class_type ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = ct} ] ]
  ;
  class_type_declaration:
    [ [ vf = OPT "virtual"; n = LIDENT; ctp = class_type_parameters; "=";
        cs = class_type ->
          {MLast.ciLoc = _loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = cs} ] ]
  ;
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> <:expr< new $list:i$ >> ] ]
  ;
  expr: LEVEL "."
    [ [ e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t = ctyp; ":>"; t2 = ctyp; ")" ->
          <:expr< ($e$ : $t$ :> $t2$ ) >>
      | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
      | "{<"; fel = LIST0 field_expr SEP ";"; ">}" ->
          <:expr< {< $list:fel$ >} >> ] ]
  ;
  field_expr:
    [ [ l = label; "="; e = expr -> (l, e) ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> <:ctyp< # $list:id$ >>
      | "<"; (ml, v) = meth_list; ">" -> <:ctyp< < $list:ml$ $opt:v$ > >>
      | "<"; ">" -> <:ctyp< < > >> ] ]
  ;
  meth_list:
    [ [ f = field; ";"; (ml, v) = SELF -> ([f :: ml], v)
      | f = field; ";" -> ([f], False)
      | f = field -> ([f], False)
      | ".." -> ([], True) ] ]
  ;
  field:
    [ [ lab = LIDENT; ":"; t = ctyp -> (lab, t) ] ]
  ;
  typevar:
    [ [ "'"; i = ident -> i ] ]
  ;
  clty_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  class_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "["; "="; rfl = row_field_list; "]" ->
          <:ctyp< [ = $list:rfl$ ] >>
      | "["; ">"; rfl = row_field_list; "]" ->
          <:ctyp< [ > $list:rfl$ ] >>
      | "["; "<"; rfl = row_field_list; "]" ->
          <:ctyp< [ < $list:rfl$ ] >>
      | "["; "<"; rfl = row_field_list; ">"; ntl = LIST1 name_tag; "]" ->
          <:ctyp< [ < $list:rfl$ > $list:ntl$ ] >>
      | "[<"; rfl = row_field_list; "]" ->
          <:ctyp< [ < $list:rfl$ ] >>
      | "[<"; rfl = row_field_list; ">"; ntl = LIST1 name_tag; "]" ->
          <:ctyp< [ < $list:rfl$ > $list:ntl$ ] >> ] ]
  ;
  row_field_list:
    [ [ rfl = LIST0 row_field SEP "|" -> rfl ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> <:row_field< ` $i$ >>
      | "`"; i = ident; "of"; ao = OPT "&"; l = LIST1 ctyp SEP "&" ->
          <:row_field< ` $i$ of $opt:o2b ao$ $list:l$ >>
      | t = ctyp -> <:row_field< $t$ >> ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> <:patt< ` $s$ >>
      | "#"; sl = mod_ident -> <:patt< # $list:sl$ >>
      | i = TILDEIDENT; ":"; p = SELF -> <:patt< ~ $i$ : $p$ >>
      | i = LABEL; p = SELF -> <:patt< ~ $i$ : $p$ >>
      | i = TILDEIDENT -> <:patt< ~ $i$ >>
      | i = QUESTIONIDENT; ":"; "("; p = patt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? $i$ : ($p$ $opt:eo$) >>
      | i = OPTLABEL; "("; p = patt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? $i$ : ($p$ $opt:eo$) >>
      | i = QUESTIONIDENT ->
          <:patt< ? $i$ >>
      | "?"; "("; p = patt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? ($p$ $opt:eo$) >> ] ]
  ;
  patt_tcon:
    [ [ p = patt; ":"; t = ctyp -> <:patt< ($p$ : $t$) >>
      | p = patt -> p ] ]
  ;
  ipatt:
    [ [ i = TILDEIDENT; ":"; p = SELF -> <:patt< ~ $i$ : $p$ >>
      | i = LABEL; p = SELF -> <:patt< ~ $i$ : $p$ >>
      | i = TILDEIDENT -> <:patt< ~ $i$ >>
      | i = QUESTIONIDENT; ":"; "("; p = ipatt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? $i$ : ($p$ $opt:eo$) >>
      | i = OPTLABEL; "("; p = ipatt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? $i$ : ($p$ $opt:eo$) >>
      | i = QUESTIONIDENT ->
          <:patt< ? $i$ >>
      | "?"; "("; p = ipatt_tcon; eo = OPT eq_expr; ")" ->
          <:patt< ? ($p$ $opt:eo$) >> ] ]
  ;
  ipatt_tcon:
    [ [ p = ipatt; ":"; t = ctyp -> <:patt< ($p$ : $t$) >>
      | p = ipatt -> p ] ]
  ;
  eq_expr:
    [ [ "="; e = expr -> e ] ]
  ;
  expr: AFTER "apply"
    [ "label" NONA
      [ i = TILDEIDENT; ":"; e = SELF -> <:expr< ~ $i$ : $e$ >>
      | i = LABEL; e = SELF -> <:expr< ~ $i$ : $e$ >>
      | i = TILDEIDENT -> <:expr< ~ $i$ >>
      | i = QUESTIONIDENT; ":"; e = SELF -> <:expr< ? $i$ : $e$ >>
      | i = OPTLABEL; e = SELF -> <:expr< ? $i$ : $e$ >>
      | i = QUESTIONIDENT -> <:expr< ? $i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> <:expr< ` $s$ >> ] ]
  ;
  direction_flag:
    [ [ "to" -> True
      | "downto" -> False ] ]
  ;
  (* Compatibility old syntax of variant types definitions *)
  ctyp: LEVEL "simple"
    [ [ "[|"; warning_variant; rfl = row_field_list; "|]" ->
          <:ctyp< [ = $list:rfl$ ] >>
      | "[|"; warning_variant; ">"; rfl = row_field_list; "|]" ->
          <:ctyp< [ > $list:rfl$ ] >>
      | "[|"; warning_variant; "<"; rfl = row_field_list; "|]" ->
          <:ctyp< [ < $list:rfl$ ] >>
      | "[|"; warning_variant; "<"; rfl = row_field_list; ">";
        ntl = LIST1 name_tag; "|]" ->
          <:ctyp< [ < $list:rfl$ > $list:ntl$ ] >> ] ]
  ;
  warning_variant:
    [ [ -> warn_variant _loc ] ]
  ;
  (* Compatibility old syntax of sequences *)
  expr: LEVEL "top"
    [ [ "do"; seq = LIST0 [ e = expr; ";" -> e ]; "return"; warning_sequence;
        e = SELF ->
          <:expr< do { $list:append_elem seq e$ } >>
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; seq = LIST0 [ e = expr; ";" -> e ]; warning_sequence; "done" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:seq$ } >>
      | "while"; e = SELF; "do"; seq = LIST0 [ e = expr; ";" -> e ];
        warning_sequence; "done" ->
          <:expr< while $e$ do { $list:seq$ } >> ] ]
  ;
  warning_sequence:
    [ [ -> warn_sequence _loc ] ]
  ;
END;

EXTEND
  GLOBAL: interf implem use_file top_phrase expr patt;
  interf:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:sig_item< # $n$ $opt:dp$ >>, _loc)], True)
      | si = sig_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  sig_item_semi:
    [ [ si = sig_item; ";" -> (si, _loc) ] ]
  ;
  implem:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([(<:str_item< # $n$ $opt:dp$ >>, _loc)], True)
      | si = str_item_semi; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  str_item_semi:
    [ [ si = str_item; ";" -> (si, _loc) ] ]
  ;
  top_phrase:
    [ [ ph = phrase -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          ([<:str_item< # $n$ $opt:dp$ >>], True)
      | si = str_item; ";"; (sil, stopped) = SELF -> ([si :: sil], stopped)
      | EOI -> ([], False) ] ]
  ;
  phrase:
    [ [ "#"; n = LIDENT; dp = OPT expr; ";" ->
          <:str_item< # $n$ $opt:dp$ >>
      | sti = str_item; ";" -> sti ] ]
  ;
  expr: LEVEL "simple"
    [ [ x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({ (Lexing.dummy_pos) with Lexing.pos_cnum = int_of_string (String.sub x 0 i) }
                 ,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> ({(Lexing.dummy_pos) with Lexing.pos_cnum = 0}, x) ]
          in
          Pcaml.handle_expr_locate _loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation _loc x ] ]
  ;
  patt: LEVEL "simple"
    [ [ x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({(Lexing.dummy_pos) with Lexing.pos_cnum = int_of_string (String.sub x 0 i)}
                 ,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> ({(Lexing.dummy_pos) with Lexing.pos_cnum = 0}, x) ]
          in
          Pcaml.handle_patt_locate _loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_patt_quotation _loc x ] ]
  ;
END;
