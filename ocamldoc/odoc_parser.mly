%{
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Odoc_comments_global

%}

%token <string * (string option)> Description

%token <string> See_url
%token <string> See_file
%token <string> See_doc

%token <string> T_PARAM
%token T_AUTHOR
%token T_VERSION
%token T_SEE
%token T_SINCE
%token <string> T_BEFORE
%token T_DEPRECATED
%token <string> T_RAISES
%token T_RETURN
%token <string> T_CUSTOM

%token EOF

%token <string> Desc

/* Start Symbols */
%start main info_part2 see_info
%type <(string * (string option)) option> main
%type <unit> info_part2
%type <Odoc_types.see_ref * string> see_info


%%
see_info:
  see_ref Desc { ($1, $2) }
;

see_ref:
    See_url { Odoc_types.See_url $1 }
| See_file { Odoc_types.See_file $1 }
| See_doc { Odoc_types.See_doc $1 }
;

main:
  Description { Some $1 }
| EOF { None }
;

info_part2:
  element_list EOF { () }
;

element_list:
  element { () }
| element element_list { () }
;

element:
| param { () }
| author { () }
| version { () }
| see { () }
| since { () }
| before { () }
| deprecated { () }
| raise_exc { () }
| return { () }
| custom { () }
;

param:
    T_PARAM Desc { params := !params @ [($1, $2)] }
;
author:
    T_AUTHOR Desc { authors := !authors @ [ $2 ] }
;
version:
    T_VERSION Desc { version := Some $2 }
;
see:
    T_SEE Desc { sees := !sees @ [$2] }
;
since:
    T_SINCE Desc { since := Some $2 }
;
before:
    T_BEFORE Desc { before := !before @ [($1, $2)] }
;
deprecated:
    T_DEPRECATED Desc { deprecated := Some $2 }
;
raise_exc:
    T_RAISES Desc
    {  raised_exceptions := !raised_exceptions @ [($1, $2)] }
;
return:
    T_RETURN Desc { return_value := Some $2 }
;

custom:
    T_CUSTOM Desc { customs := !customs @ [($1, $2)] }
;


%%
