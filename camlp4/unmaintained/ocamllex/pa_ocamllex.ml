(* pa_o.cmo q_MLast.cmo pa_extend.cmo pr_dump.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                               Camlp4                                *)
(*                                                                     *)
(*     Alain Frisch, projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file                 *)
(*   ../../../LICENSE.                                                 *)
(*                                                                     *)
(***********************************************************************)
(* $Id$ *)

open Syntax
open Lexgen
open Compact

(* Adapted from output.ml *)
(**************************)

(* Output the DFA tables and its entry points *)

(* To output an array of short ints, encoded as a string *)

let output_byte buf b =
  Buffer.add_char buf '\\';
  Buffer.add_char buf (Char.chr(48 + b / 100));
  Buffer.add_char buf (Char.chr(48 + (b / 10) mod 10));
  Buffer.add_char buf (Char.chr(48 + b mod 10))

let loc = (Lexing.dummy_pos,Lexing.dummy_pos)

let output_array v =
  let b = Buffer.create (Array.length v * 3) in
  for i = 0 to Array.length v - 1 do
    output_byte b (v.(i) land 0xFF);
    output_byte b ((v.(i) asr 8) land 0xFF);
    if i land 7 = 7 then Buffer.add_string b "\\\n    "
  done;
  let s = Buffer.contents b in
  <:expr< $str:s$ >>

let output_byte_array v =
  let b = Buffer.create (Array.length v * 2) in
  for i = 0 to Array.length v - 1 do
    output_byte b (v.(i) land 0xFF);
    if i land 15 = 15 then Buffer.add_string b "\\\n    "
  done;
  let s = Buffer.contents b in
  <:expr< $str:s$ >>



(* Output the tables *)

let output_tables tbl =
  <:str_item< value lex_tables = {
    Lexing.lex_base = $output_array tbl.tbl_base$;
    Lexing.lex_backtrk = $output_array tbl.tbl_backtrk$;
    Lexing.lex_default = $output_array tbl.tbl_default$;
    Lexing.lex_trans = $output_array tbl.tbl_trans$;
    Lexing.lex_check = $output_array tbl.tbl_check$;
    Lexing.lex_base_code = $output_array tbl.tbl_base_code$;
    Lexing.lex_backtrk_code = $output_array tbl.tbl_backtrk_code$;
    Lexing.lex_default_code = $output_array tbl.tbl_default_code$;
    Lexing.lex_trans_code = $output_array tbl.tbl_trans_code$;
    Lexing.lex_check_code = $output_array tbl.tbl_check_code$;
    Lexing.lex_code = $output_byte_array tbl.tbl_code$
  } >>

(* Output the entries *)

let rec make_alias n = function
  | [] -> []
  | h::t -> 
      (h, "__ocaml_lex_arg_" ^ (string_of_int n)) :: (make_alias (succ n) t)

let abstraction =
  List.fold_right (fun (p,a) e -> <:expr< fun ($p$ as $lid:a$) -> $e$ >>)


let application =
  List.fold_left (fun f (_,a) -> <:expr< $f$ $lid:a$ >>)

let int i = <:expr< $int:string_of_int i$ >>

let output_memory_actions acts = 
  let aux = function
    | Copy (tgt, src) -> 
	<:expr< lexbuf.Lexing.lex_mem.($int tgt$) := 
      lexbuf.Lexing.lex_mem.($int src$) >>
    | Set tgt ->
        <:expr< lexbuf.Lexing.lex_mem.($int tgt$) := 
      lexbuf.Lexing.lex_curr_pos >>
  in
  <:expr< do { $list:List.map aux acts$ } >>

let output_base_mem = function
  | Mem i -> <:expr< lexbuf.Lexing.lex_mem.($int i$) >>
  | Start -> <:expr< lexbuf.Lexing.lex_start_pos >>
  | End   -> <:expr< lexbuf.Lexing.lex_curr_pos >>

let output_tag_access = function
  | Sum (a,0) -> output_base_mem a
  | Sum (a,i) -> <:expr< $output_base_mem a$ + $int i$ >>

let rec output_env e = function
  | [] -> e
  | (x, Ident_string (o,nstart,nend)) :: rem ->
      <:expr< 
	  let $lid:x$ = 
	    Lexing.$lid:if o then "sub_lexeme_opt" else "sub_lexeme"$
	    lexbuf $output_tag_access nstart$ $output_tag_access nend$
          in $output_env e rem$
      >>
  | (x, Ident_char (o,nstart)) :: rem ->
      <:expr< 
	  let $lid:x$ = 
	    Lexing.$lid: if o then "sub_lexeme_char_opt" else "sub_lexeme_char"$
	    lexbuf $output_tag_access nstart$
          in $output_env e rem$
      >>

let output_entry e =
  let init_num, init_moves = e.auto_initial_state in
  let args = make_alias 0 (e.auto_args @ [ <:patt< lexbuf >> ]) in
  let f = "__ocaml_lex_rec_" ^ e.auto_name ^ "_rec" in
  let call_f = application <:expr< $lid:f$ >> args in
  let body_wrapper = 
    <:expr< 
      do {
	lexbuf.Lexing.lex_mem := Array.create $int e.auto_mem_size$ (-1) ;
	$output_memory_actions init_moves$;
        $call_f$ $int init_num$
      } >> in
  let cases = 
    List.map
      (fun (num, env, (loc,e)) ->
         <:patt< $int:string_of_int num$ >>, 
	 None, 
	 output_env <:expr< $e$ >> env
	     (* Note: the <:expr<...>> above is there to set the location *)
      ) e.auto_actions @
    [ <:patt< __ocaml_lex_n >>,
      None,
      <:expr< do 
        { lexbuf.Lexing.refill_buff lexbuf; $call_f$ __ocaml_lex_n  }>> ]
  in
  let engine = 
    if e.auto_mem_size = 0 
    then <:expr< Lexing.engine >>
    else <:expr< Lexing.new_engine >> in
  let body = 
    <:expr< fun state ->
      match $engine$ lex_tables state lexbuf with [ $list:cases$ ] >> in
  [
    <:patt< $lid:e.auto_name$ >>, (abstraction args body_wrapper);
    <:patt< $lid:f$ >>, (abstraction args body) 
  ]

(* Main output function *)

exception Table_overflow

let output_lexdef tables entry_points =
  Printf.eprintf 
    "pa_ocamllex: lexer found; %d states, %d transitions, table size %d bytes\n"
    (Array.length tables.tbl_base)
    (Array.length tables.tbl_trans)
    (2 * (Array.length tables.tbl_base + Array.length tables.tbl_backtrk +
          Array.length tables.tbl_default + Array.length tables.tbl_trans +
          Array.length tables.tbl_check));
  let size_groups =
    (2 * (Array.length tables.tbl_base_code +
          Array.length tables.tbl_backtrk_code +
          Array.length tables.tbl_default_code +
          Array.length tables.tbl_trans_code +
          Array.length tables.tbl_check_code) +
    Array.length tables.tbl_code) in
  if  size_groups > 0 then
    Printf.eprintf "pa_ocamllex: %d additional bytes used for bindings\n" 
      size_groups ;
  flush stderr;
  if Array.length tables.tbl_trans > 0x8000 then raise Table_overflow;

  let entries = List.map output_entry entry_points in
  [output_tables tables; <:str_item< value rec $list:List.flatten entries$ >> ]


(* Adapted from parser.mly and main.ml *)
(***************************************)

(* Auxiliaries for the parser. *)

let char s = Char.code (Token.eval_char s)

let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then
      Characters (Cset.singleton (Char.code s.[n]))
    else
      Sequence
        (Characters(Cset.singleton (Char.code s.[n])),
         re_string (succ n))
  in re_string 0

let char_class c1 c2 = Cset.interval c1 c2

let all_chars = Cset.all_chars

let rec remove_as = function
  | Bind (e,_) -> remove_as e
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) -> Sequence (remove_as e1, remove_as e2)
  | Alternative (e1, e2) -> Alternative (remove_as e1, remove_as e2)
  | Repetition e -> Repetition (remove_as e)

let () =
  Hashtbl.add named_regexps "eof" (Characters Cset.eof)

(* The parser *)

let let_regexp = Grammar.Entry.create Pcaml.gram "pa_ocamllex let"
let header = Grammar.Entry.create Pcaml.gram "pa_ocamllex header"
let lexer_def = Grammar.Entry.create Pcaml.gram "pa_ocaml lexerdef"

EXTEND
 GLOBAL: Pcaml.str_item let_regexp header lexer_def;

 let_regexp: [
   [ x = LIDENT; "="; r = regexp ->
       if Hashtbl.mem named_regexps x then
         Printf.eprintf 
           "pa_ocamllex (warning): multiple definition of named regexp '%s'\n"
           x;
       Hashtbl.add named_regexps x r;
   ]
 ];

 lexer_def: [
   [ def = LIST0 definition SEP "and" ->
       (try
          let (entries, transitions) = make_dfa def in
          let tables = compact_tables transitions in
          let output = output_lexdef tables entries in
          <:str_item< declare $list: output$ end >> 
        with 
	  |Table_overflow ->
             failwith "Transition table overflow in lexer, automaton is too big"
	  | Lexgen.Memory_overflow ->
              failwith "Position memory overflow in lexer, too many as variables")
   ]
 ];


 Pcaml.str_item: [
   [ "pa_ocamllex"; LIDENT "rule"; d = lexer_def -> d
   | "pa_ocamllex"; "let"; let_regexp -> 
       <:str_item< declare $list: []$ end >>
   ]
 ];
 
 definition: [
   [ x=LIDENT; pl = LIST0 Pcaml.patt LEVEL "simple"; "=";
     short=[ LIDENT "parse" -> false | LIDENT "shortest" -> true ];
     OPT "|"; l = LIST0 [ r=regexp; a=action -> (r,a) ] SEP "|" ->
     { name=x ; shortest=short ; args=pl ; clauses = l } ]
 ];

 action: [
   [ "{"; e = OPT Pcaml.expr; "}" -> 
       let e = match e with
         | Some e -> e
         | None -> <:expr< () >>
       in
       (loc,e)
   ]
 ];

 header:  [
   [ "{"; e = LIST0 [ si = Pcaml.str_item; OPT ";;" -> si ]; "}" -> 
       [<:str_item< declare $list:e$ end>>, loc] ]
   | [ -> [] ]
 ];

 regexp: [
   [ r = regexp; "as"; i = LIDENT -> Bind (r,i) ]
 | [ r1 = regexp; "|"; r2 = regexp -> Alternative(r1,r2) ]
 | [ r1 = regexp; r2 = regexp -> Sequence(r1,r2) ]
 | [ r = regexp; "*" -> Repetition r
   | r = regexp; "+" -> Sequence(Repetition (remove_as r), r)
   | r = regexp; "?" -> Alternative(Epsilon, r)
   | "("; r = regexp; ")" -> r
   | "_" -> Characters all_chars
   | c = CHAR -> Characters (Cset.singleton (char c))
   | s = STRING -> regexp_for_string (Token.eval_string loc s)
   | "["; cc = ch_class; "]" ->  Characters cc
   | x = LIDENT ->
       try  Hashtbl.find named_regexps x
       with Not_found ->
         failwith 
           ("pa_ocamllex (error): reference to unbound regexp name `"^x^"'")
   ]
 ];

 ch_class: [
   [ "^"; cc = ch_class -> Cset.complement cc]
 | [ c1 = CHAR; "-"; c2 = CHAR -> Cset.interval (char c1) (char c2)
   | c = CHAR -> Cset.singleton (char c)
   | cc1 = ch_class; cc2 = ch_class -> Cset.union cc1 cc2
   ]
 ];
END

(* We have to be careful about "rule"; in standalone mode,
   it is used as a keyword (otherwise, there is a conflict
   with named regexp); in normal mode, it is used as LIDENT
   (we do not want to reserve such an useful identifier).

   Plexer does not like identifiers used as keyword _and_
   as LIDENT ...
*)

let standalone =
  let already = ref false in
  fun () ->
    if not (!already) then
    begin
      already := true;
      Printf.eprintf "pa_ocamllex: stand-alone mode\n";

      DELETE_RULE Pcaml.str_item: "pa_ocamllex"; LIDENT "rule";lexer_def END;
      DELETE_RULE Pcaml.str_item: "pa_ocamllex"; "let"; let_regexp END;
      let ocamllex = Grammar.Entry.create Pcaml.gram "pa_ocamllex" in
      EXTEND GLOBAL: ocamllex let_regexp header lexer_def;
      ocamllex: [
        [ h = header;
          l  = [LIST0 ["let"; let_regexp]; "rule"; d = lexer_def -> (d,loc)];
          t = header; EOI -> h @ (l :: t) ,false
        ]
      ];
      END;
      Pcaml.parse_implem := Grammar.Entry.parse ocamllex
    end

let () =
  Pcaml.add_option "-ocamllex" (Arg.Unit standalone)
    "Activate (standalone) ocamllex emulation mode."

