/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of Objective Caml            */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file ../LICENSE.                                      */
/*                                                                     */
/***********************************************************************/

%{
open Code
%}

%token <string> IFDEF
%token <string> IFNDEF
%token ELSE
%token ENDIF
%token <string> DEFINE
%token <string> UNDEF
%token <string> OTHER
%token EOF

/* entry */

%start code_list
%type <Code.code list> code_list

%%

code_list:
    /* empty */ { [] }
  | code code_list { $1 :: $2 }
;

code:
  | DEFINE { Define $1 }
  | UNDEF { Undef $1 }
  | IFDEF code_list ELSE code_list ENDIF { Ifdef (true, $1, $2, Some ($4)) }
  | IFNDEF code_list ELSE code_list ENDIF { Ifdef (false, $1, $2, Some ($4)) }
  | IFDEF code_list ENDIF { Ifdef (true, $1, $2, None) }
  | IFNDEF code_list ENDIF { Ifdef (false, $1, $2, None) }
  | OTHER { Line $1 }
;

%%
