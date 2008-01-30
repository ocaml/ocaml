{
open Lexing

type token =
  | MetaChar of char
  | Group of string option * token list
  | Escaped of string
  | Quoted of string
  | Quantifier of bool (* greedieness *) * int * int * token
  | Char of char
  | CharClass of bool (* pos/neg *) * charclass list
  | Comment of string
  | Modifier of string
  | Cluster of string * token list
  | LookAhead of bool (* pos/neg *) * token list
  | LookBehind of bool (* pos/neg *) * token list
  | Independent of token list
  | PBackRef of string
  | Recursive of string option (* None = entire *)
  | CallOut of int
  | CallOutByName of string

and charclass = 
  | ClassChar of token
  | ClassPOSIX of bool (* pos/neg *) * string
  | ClassRange of token * token
  | ClassQuoted of string

let char_lexeme lexbuf = (lexeme lexbuf).[0]

exception Error of int * int * string

let error lexbuf s =
  raise (Error (lexeme_start lexbuf, lexeme_end lexbuf, s))

let posix_character_classes = [
  "alpha"; "alnum"; "ascii"; "blank"; "cntrl"; "digit"; "graph";
  "lower"; "print"; "punct"; "space"; "upper"; "word"; "xdigit"
]

let get_class_name start lexbuf =
  let lxm = lexeme lexbuf in
  let classname = String.sub lxm start (String.length lxm - (start + 2)) in
  if not (List.mem classname posix_character_classes) then
    error lexbuf "unknown POSIX class"
  else classname
}

let digit = ['0'-'9']
let hex = ['0'-'9' 'A'-'F' 'a'-'f']

(* 
   not yet:
   - Comment by # ... \n in PCRE_EXTENDED mode
   - \x{hhhh} is special even not in UTF-8 mmode
   - various PCRE_... mode
   - (?(condition)yes-pattern|no-pattern)
   - (?(condition)yes-pattern)
   - UTF8
   
   not supported by PCRE:

   (?{ code })
   (??{ code })
   
*)

rule tokens = parse
  | "\\Q"
      {
        let re = Quoted (quoted lexbuf (Buffer.create 16)) in
	let res = tokens lexbuf in
	re :: res
      }
      (* do not consider about \E not paired with \Q.
	 it is treated as a Escaped "E". *)
      
  | '\\' 
      { 
	let re = quantifier lexbuf (escaped lexbuf) in
	let res = tokens lexbuf in
	re :: res
      }

  | '.' 
      {
        let re = quantifier lexbuf (MetaChar '.') in
	let res = tokens lexbuf in
	re :: res
      }
  | ['^' '$' '|'] 
      {
        let re = MetaChar (char_lexeme lexbuf) in
	let res = tokens lexbuf in
	re :: res
     }
  | '(' 
      {
        let re = quantifier lexbuf (Group (None, group lexbuf)) in
	let res = tokens lexbuf in
	re :: res 
      }
  | "(?P<" (['a'-'z'] (digit | ['A'-'Z' 'a'-'z' '_'])*) ">" 
      (* caml restriction: the first char must be lower case. *) 
      {
         (* python style named pattern *)
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 4 (String.length lxm - 5) in
	 let re = Group (Some name, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?P=" (['a'-'z'] (digit | ['A'-'Z' 'a'-'z' '_'])*) ")" 
      (* caml restriction: the first char must be lower case. *) 
      {
         (* python style back reference *)
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 4 (String.length lxm - 5) in
	 let re = PBackRef name in
	 let res = tokens lexbuf in
	 re :: res
      }

(* Recursive *)
  | "(?P" digit+ ")"
      {
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 3 (String.length lxm - 4) in
	 let re = Recursive (Some name) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?R)" 
      {
	 let re = Recursive None in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?P>" (['a'-'z'] (digit | ['A'-'Z' 'a'-'z' '_'])*) ")" 
      (* caml restriction: the first char must be lower case. *) 
      {
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 4 (String.length lxm - 5) in
	 let re = Recursive (Some name) in
	 let res = tokens lexbuf in
	 re :: res
      }

  (* Call-out *)
  | "(?C" ['1'-'9'] digit* ")" (* callout id must be positive *)
      {
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 3 (String.length lxm - 4) in
	 let pos = 
	   match name with
	   | "" -> 0
	   | _ -> int_of_string name
	 in
	 let re = CallOut pos in
	 let res = tokens lexbuf in
	 re :: res
      }

  (* Call-out-by-name: Caml extension *)
  | "(?C" ['a'-'z' '_'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* ")"
      {
         let lxm = lexeme lexbuf in
	 let name = String.sub lxm 3 (String.length lxm - 4) in
	 let re = CallOutByName name in
	 let res = tokens lexbuf in
	 re :: res
      }

  (* Comment *)
  | "(?#" [^ ')']* ')'
      {
         let lxm = lexeme lexbuf in
	 let re = Comment (String.sub lxm 3 (String.length lxm - 4)) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?" ['i' 'm' 's' 'x']* ("-" ['i' 'm' 's' 'x']*)? ")"
      {
         let lxm = lexeme lexbuf in
	 let re = Modifier (String.sub lxm 2 (String.length lxm - 3)) in
	 let res = tokens lexbuf in
	 re :: res
      }
 
  | "(?" ['i' 'm' 's' 'x']* ("-" ['i' 'm' 's' 'x']*)? ":"
      {
         let lxm = lexeme lexbuf in
	 let pos = String.index lxm ':' in
	 let md = String.sub lxm 2 (pos - 2) in
	 let re = Cluster(md, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?="
      {
	 let re = LookAhead(true, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?!"
      {
	 let re = LookAhead(false, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?<="
      {
	 let re = LookBehind(true, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?<!"
      {
	 let re = LookBehind(false, group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
  | "(?>"
      {
	 let re = Independent (group lexbuf) in
	 let res = tokens lexbuf in
	 re :: res
      }
 
  | '[' 
      {
       let re = quantifier lexbuf (CharClass (true, charclass lexbuf)) in
	let res = tokens lexbuf in
	re :: res 
      }
  | "[^"
      {
       let re = quantifier lexbuf (CharClass (false, charclass lexbuf)) in
	let res = tokens lexbuf in
	re :: res 
      }
  | ['*' '+' '?']
      {
        (* quantifiers cannot go here *)
        error lexbuf "Quantifier follows nothing in regex"
      }
  | _ # ['\\' '.' '^' '$' '|' '(' ')' '[' '*' '+' '?'] 
		  (* { is a metachar, but maybe parsed as a literal, too *) 
      {
        let re = quantifier lexbuf (Char (char_lexeme lexbuf)) in
	let res = tokens lexbuf in
	re :: res
      }
  | "" { [] }
      
and quantifier = parse
  | '*' {(fun tk -> Quantifier (true, 0, -1, tk))}
  | '+' {(fun tk -> Quantifier (true, 1, -1, tk))}
  | '?' {(fun tk -> Quantifier (true, 0, 1, tk))}
  | "*?" {(fun tk -> Quantifier (false, 0, -1, tk))}
  | "+?" {(fun tk -> Quantifier (false, 1, -1, tk))}
  | "??" {(fun tk -> Quantifier (false, 0, 1, tk))}
  | '{' digit+ '}' 
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let d = int_of_string (String.sub lxm 1 (String.length lxm - 2)) in
	  Quantifier (true, d, d, tk)
      }
  | '{' digit+ ",}"
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let d = int_of_string (String.sub lxm 1 (String.length lxm - 3)) in
	  Quantifier (true, d, -1, tk)
      }
  | '{' digit+ ',' digit+ '}'
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let pos = String.index lxm ',' in
	  let d1 = 
	    int_of_string (String.sub lxm 1 (pos - 1)) 
	  in
	  let d2 = 
	    int_of_string 
	      (String.sub lxm (pos + 1) (String.length lxm - pos - 2)) 
	  in
	  Quantifier (true, d1, d2, tk)
      }
  | '{' digit+ "}?"
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let d = int_of_string (String.sub lxm 1 (String.length lxm - 3)) in
	  Quantifier (false, d, d, tk)
      }
  | '{' digit+ ",}?"
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let d = int_of_string (String.sub lxm 1 (String.length lxm - 4)) in
	  Quantifier (false, d, -1, tk)
      }
  | '{' digit+ ',' digit+ "}?"
      {
        fun tk ->
          let lxm = lexeme lexbuf in
	  let pos = String.index lxm ',' in
	  let d1 = 
	    int_of_string (String.sub lxm 1 (pos - 1)) 
	  in
	  let d2 = 
	    int_of_string 
	      (String.sub lxm (pos + 1) (String.length lxm - pos - 3)) 
	  in
	  Quantifier (false, d1, d2, tk)
      }
  | "" {(fun tk -> tk)}

and group = parse
  | ')' { [] }
  | "" 
      {
        let res = tokens lexbuf in
	let res' = group lexbuf in
	res @ res' 
      }

and quoted = parse
  | "" | "\\E" { fun buf -> Buffer.contents buf }
  | "\\" 
  | [^ '\\']*
      { 
	fun buf -> 
	  Buffer.add_string buf (lexeme lexbuf);
	  quoted lexbuf buf
      }

and charclass = parse
  | ']' { [] }
  | "[:" [^ ':']+ ":]" 
      { 
	let cls = ClassPOSIX (true, get_class_name 2 lexbuf) in
	cls :: charclass lexbuf
      }
  | "[:^" [^ ':']+ ":]" 
      { 
	let cls = ClassPOSIX (false, get_class_name 3 lexbuf) in
	cls :: charclass lexbuf
      }
  | "\\Q"
      {
       let cls = ClassQuoted (quoted lexbuf (Buffer.create 16)) in
	cls :: charclass lexbuf
      }
  | '\\' 
      {
        let e = escaped lexbuf in
	let clses = range lexbuf e in
	clses @ charclass lexbuf 
      }
  | [^ '[' '\\' ']']
      {
        let s = char_lexeme lexbuf in
	let clses =  range lexbuf (Char s) in
	clses @ charclass lexbuf
      }
  | "" { error lexbuf "Unmatched [ in regex" }
          
and range = parse
  | "-[:" [^ ':']+ ":]"
  | "-[:^" [^ ':']+ ":]"
  | '-' ("\\w" | "\\W" | "\\s" | "\\S" | "\\d" | "\\D")
      {
        (* not a range *)
        fun c ->
	  ClassChar c :: 
	  charclass (from_string (lexeme lexbuf ^ "]" (* to finish *)))
      }
      
  | "-\\" 
      {
        fun c ->
	  let e = escaped lexbuf in
	  [ClassRange (c, e)]
      }

  | '-' [^ '[' '\\' ']']
      {
        fun c ->
	  let c' = Char (lexeme lexbuf).[1] in
	  [ClassRange (c, c')]
      }
  | "" { fun c -> [ClassChar c] }
      

and escaped = parse
  | digit (* backreference *)
  | digit digit (* backreference or \0dd *)
  | digit digit digit (* same, same *)
  | "x" 
  | "x" hex
  | "x" hex hex
  | "x{" hex* "}"
  | "c" [^ '\\' '/'] | "\\c\\" _
  | "N{" [^ '}']* "}"
  | ['p' 'P'] [^ '\\' '/' '{'] | "\\" ['p' 'P'] "\\" _
  | ['p' 'P'] "{" [^ '}']+ "}"
  | _
      { 
	let lxm = lexeme lexbuf in
	if lxm = "Q" then assert false; (* must be treated differently *)
	Escaped lxm
      }
  | "" { error lexbuf "Search pattern not terminated" }

{
let from_string s =
  let lexbuf = Lexing.from_string s in
  let regexp = tokens lexbuf in
  let endpos = lexeme_end lexbuf in
  if endpos < String.length s then
    raise (Error (endpos, endpos + 1,
		  Printf.sprintf "Unmatched %c" s.[endpos]))
  else regexp

type typ = {
    num_of_groups : int;
    named_groups : (string * int) list;
    callouts : int list; (* 255 : default, not in the list *)
    named_callouts : (string * int) list; (* caml extension *)
  }

exception Double_defined_group of string
exception Too_many_callouts 

let type_regexp re =

  let pos = ref 0 in
  let named_groups = ref [] in
  let callouts = ref [] in
  let named_callouts = ref [] in

  let rec regexp re = List.iter token re
  and token = function
    | Group (Some n, re) -> 
	if List.mem_assoc n !named_groups then 
	  raise (Double_defined_group n);
	incr pos;
	named_groups := (n, !pos) :: !named_groups;
	regexp re
    | Group (None, re) -> 
	incr pos;
	regexp re
    | Quantifier (_, _, _, tkn) -> token tkn
    | Cluster (_, re)
    | LookAhead (_, re)
    | LookBehind (_, re)
    | Independent re -> regexp re
    | CallOut n -> callouts := n :: !callouts
    | CallOutByName s -> named_callouts := s :: !named_callouts
    | Char _
    | MetaChar _
    | Escaped _
    | Quoted _
    | Comment _
    | CharClass _
    | Modifier _
    | PBackRef _
    | Recursive _ -> ()
  in

  let pos = ref 1 in
  let rec give_numbers_for_named_callouts = function
    | [] -> []
    | s::ss ->
	while List.mem !pos !callouts do incr pos done;
	if !pos = 255 then raise Too_many_callouts;
	(s, !pos) :: give_numbers_for_named_callouts ss
  in

  regexp re;
  { num_of_groups= !pos;
    named_groups= !named_groups;
    callouts= !callouts;
    named_callouts= give_numbers_for_named_callouts !named_callouts }

let string_of_tokens typ tkns =
  let buf = Buffer.create 16 in
  let char c = Buffer.add_char buf c in
  let string s = Buffer.add_string buf s in
  let greedy = function
    | true -> char '?'
    | false -> ()
  in

  let rec token = function
    | MetaChar c -> char c
    | Group (None, tkns) -> 
	char '(';
	List.iter token tkns;
	char ')'
    | Group (Some s, tkns) -> 
	string "(?P<";
	string s;
	char '>';
	List.iter token tkns;
	char ')'
    | Escaped s ->
	char '\\';
	string s
    | Quoted s ->
	string "\\Q";
	string s;
	string "\\E"
    | Quantifier (b, 0, -1, tk) ->
	token tk;
	char '*';
	greedy b
    | Quantifier (b, 1, -1, tk) ->
	token tk;
	char '+';
	greedy b
    | Quantifier (b, 0, 1, tk) ->
	token tk;
	char '?';
	greedy b
    | Quantifier (b, n, -1, tk) ->
	token tk;
	char '{';
	string (string_of_int n);
	char ',';
	char '}';
	greedy b
    | Quantifier (b, n, m, tk) when n = m ->
	token tk;
	char '{';
	string (string_of_int n);
	char '}';
	greedy b
    | Quantifier (b, n, m, tk) ->
	token tk;
	char '{';
	string (string_of_int n);
	char ',';
	string (string_of_int m);
	char '}';
	greedy b
    | Char c -> char c
    | CharClass (b, clses) ->
	char '[';
	if not b then char '^';
	List.iter cls clses;
	char ']'
    | Comment s ->
	(* FIXME: extended mode *)
	string "(?#";
	string s;
	char ')'
    | Modifier s ->
	string "(?";
	string s;
	char ')'
    | Cluster (md, tkns) ->
	string "(?";
	string md;
	char ':';
	List.iter token tkns;
	char ')'
    | LookAhead (true, tkns) ->
	string "(?=";
	List.iter token tkns;
	char ')'
    | LookAhead (false, tkns) ->
	string "(?!";
	List.iter token tkns;
	char ')'
    | LookBehind (true, tkns) ->
	string "(?<=";
	List.iter token tkns;
	char ')'
    | LookBehind (false, tkns) ->
	string "(?<!";
	List.iter token tkns;
	char ')'
    | Independent tkns ->
	string "(?>";
	List.iter token tkns;
	char ')'
    | PBackRef s ->
	string "(?P=";
	string s;
	char ')'
    | Recursive None -> 
	string "(?R)"
    | Recursive (Some n) ->
	string "(?P";
	string n;
	char ')'
    | CallOut n ->
	string "(?C";
	string (string_of_int n);
	char ')'
    | CallOutByName s ->
	let n = List.assq s typ.named_callouts in
	string "(?C";
	string (string_of_int n);
	char ')'
  
  and cls = function 
    | ClassChar tkn -> token tkn
    | ClassPOSIX (true, s) ->
	string "[:";
	string s;
	string ":]"
    | ClassPOSIX (false, s) ->
	string "[:^";
	string s;
	string ":]"
    | ClassRange (t1, t2) ->
	token t1;
	char '-';
	token t2
    | ClassQuoted s ->
	string "\\Q";
	string s;
	string "\\E"
  in
  
  List.iter token tkns;
  Buffer.contents buf

class virtual result ty groups = 
  let named_groups = 
    List.map (fun (n,pos) -> n, Array.unsafe_get groups pos) ty.named_groups
  in
  object
  method _groups = (groups : string array)
  method _named_groups = (named_groups : (string * string) list)
  method _group n = groups.(n)
  method _unsafe_group n = Array.unsafe_get groups n
  method _named_group s = List.assoc s named_groups
  method virtual _0 : string
end

type 'a t = {
    string : string;
    typ : typ;
    result : result;
  }

let string t = t.string 

}

