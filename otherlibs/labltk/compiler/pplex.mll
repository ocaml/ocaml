(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file ../LICENSE.                                      *)
(*                                                                     *)
(***********************************************************************)

{
open Ppyac
exception Error of string
let linenum = ref 1
} 

let blank = [' ' '\013' '\009' '\012']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

rule token = parse
  blank + { token lexbuf }
| "##" [' ' '\t']* { directive lexbuf }
| ("#")? [^ '#' '\n']* '\n'? { 
       begin
         let str = Lexing.lexeme lexbuf in
         if String.length str <> 0 && str.[String.length str - 1] = '\n' then 
         begin
           incr linenum
         end;
         OTHER (str)
       end
    }
|  eof { EOF }

and directive = parse
| "ifdef" [' ' '\t']+  { IFDEF (ident lexbuf)}
| "ifndef" [' ' '\t']+  { IFNDEF (ident lexbuf)}
| "else"   { ELSE }
| "endif"  { ENDIF }
| "define" [' ' '\t']+* { DEFINE (ident lexbuf)}
| "undef"  [' ' '\t']+ { UNDEF (ident lexbuf)}
| _ { raise (Error (Printf.sprintf "unknown directive at line %d" !linenum))}

and ident = parse
| lowercase identchar* | uppercase identchar* 
    { Lexing.lexeme lexbuf }
| _ { raise (Error (Printf.sprintf "illegal identifier at line %d" !linenum)) }
