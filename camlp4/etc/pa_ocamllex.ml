(* camlp4 ./pa_o.cmo q_MLast.cmo pa_extend.cmo pr_dump.cmo *)
(* $Id$ *)
(* Alain Frisch's contribution *)

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

let loc = (-1,-1)

let output_array v =
  let b = Buffer.create (Array.length v * 3) in
  for i = 0 to Array.length v - 1 do
    output_byte b (v.(i) land 0xFF);
    output_byte b ((v.(i) asr 8) land 0xFF);
    if i land 7 = 7 then Buffer.add_string b "\\\n    "
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
    Lexing.lex_check = $output_array tbl.tbl_check$
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

let output_entry e =
  let args = make_alias 0 (<:patt< lexbuf >> :: e.auto_args) in
  let f = "__ocaml_lex_rec_" ^ e.auto_name ^ "_rec" in
  let call_f = application <:expr< $lid:f$ >> args in
  let inistate = <:expr< $int:string_of_int e.auto_initial_state$ >> in
  let cases = 
    List.map
      (fun (num, (loc,e)) ->
	 <:patt< $int:string_of_int num$ >>,
	 None, (* when ... *)
	 e
      ) e.auto_actions @
    [ <:patt< __ocaml_lex_n >>,
      None,
      <:expr< do 
	{ lexbuf.Lexing.refill_buff lexbuf; $call_f$ __ocaml_lex_n  }>> ]
  in
  [
    <:patt< $lid:e.auto_name$ >>,
    (abstraction args <:expr< $call_f$ $inistate$ >>);

    <:patt< $lid:f$ >>,
    (abstraction args <:expr< 
       fun state -> 
       match Lexing.engine lex_tables state lexbuf with 
       [ $list:cases$ ] >>)
  ]

(* Main output function *)

exception Table_overflow

let output_lexdef tables entry_points =
  Printf.eprintf 
    "pa_ocamllex: found lexer; %d states, %d transitions, table size %d bytes\n"
    (Array.length tables.tbl_base)
    (Array.length tables.tbl_trans)
    (2 * (Array.length tables.tbl_base + Array.length tables.tbl_backtrk +
          Array.length tables.tbl_default + Array.length tables.tbl_trans +
          Array.length tables.tbl_check));
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
    else if succ n = String.length s then Characters([Char.code (s.[n])])
    else Sequence(Characters([Char.code (s.[n])]), re_string (succ n))
  in re_string 0

let char_class c1 c2 =
  let rec cl n =
    if n > c2 then [] else n :: cl(succ n)
  in cl c1

let all_chars = char_class 0 255

let rec subtract l1 l2 =
  match l1 with
    [] -> []
  | a::r -> if List.mem a l2 then subtract r l2 else a :: subtract r l2

(* The parser *)

let ocamllex = Grammar.Entry.create Pcaml.gram "ocamllex"

EXTEND
 GLOBAL: Pcaml.str_item ocamllex;

 ocamllex: [
   [ h = header;
        l  = [LIST0 ["let"; let_regexp]; "rule"; d = lexer_def -> (d,loc)];
     t = header; EOI -> h @ (l :: t) ,false
   ]
 ];

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
       	with Table_overflow ->
          failwith "Transition table overflow in lexer, automaton is too big")
   ]
 ];


 Pcaml.str_item: [
   [ "rule"; d = lexer_def -> d
   | "let_regexp"; let_regexp -> <:str_item< declare $list: []$ end >>
   ]
 ];
 
 definition: [
   [ x=LIDENT; pl = LIST0 Pcaml.patt; "="; "parse"; 
     OPT "|"; l = LIST0 [ r=regexp; a=action -> (r,a) ] SEP "|" -> ((x,pl),l) ]
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
   [ r1 = regexp; "|"; r2 = regexp -> Alternative(r1,r2) ]
 | [ r1 = regexp; r2 = regexp -> Sequence(r1,r2) ]
 | [ r = regexp; "*" -> Repetition r
   | r = regexp; "+" -> Sequence(r, Repetition r)
   | r = regexp; "?" -> Alternative(r, Epsilon)
   | "("; r = regexp; ")" -> r
   | "_" -> Characters all_chars
   | "eof" -> Characters [256]
   | c = CHAR -> Characters [char c]
   | s = STRING -> regexp_for_string (Token.eval_string s)
   | "["; cc = ch_class; "]" ->  Characters cc
   | x = LIDENT ->
       try  Hashtbl.find named_regexps x
       with Not_found ->
	 failwith 
	   ("pa_ocamllex (error): reference to unbound regexp name `"^x^"'")
   ]
 ];

 ch_class: [
   [ "^"; cc = ch_class -> subtract all_chars cc]
 | [ c1 = CHAR; "-"; c2 = CHAR -> char_class (char c1) (char c2)
   | c = CHAR -> [char c]
   | cc1 = ch_class; cc2 = ch_class -> cc1 @ cc2
   ]
 ];
END


let standalone () = 
  Printf.eprintf "pa_ocamllex: stand-alone mode\n";
  Pcaml.parse_implem := Grammar.Entry.parse ocamllex

let () =
  Pcaml.add_option "-ocamllex" (Arg.Unit standalone)
    "    Activate (standalone) ocamllex emulation mode."

