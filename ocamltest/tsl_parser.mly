/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sebastien Hinderer, projet Gallium, INRIA Paris            */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Parser for the Tests Specification Language */

%{

open Location
open Tsl_ast

let mkstring s = make_string ~loc:(symbol_rloc()) s

let mkidentifier id = make_identifier ~loc:(symbol_rloc()) id

let mkenvstmt envstmt =
  make_environment_statement ~loc:(symbol_rloc()) envstmt

let ghost_pass = make_identifier ~loc:none "pass"

let wrap l =
  match l with
  | [ x ] -> x
  | _ -> Tsl_node ([], ghost_pass, [], l)

%}

%token TSL_BEGIN_C_STYLE TSL_END_C_STYLE
%token TSL_BEGIN_OCAML_STYLE TSL_END_OCAML_STYLE
%token COMMA OPEN CLOSE SEMI
%token <int> TEST_DEPTH
%token EQUAL PLUSEQUAL
/* %token COLON */
%token INCLUDE SET UNSET WITH
%token <string> IDENTIFIER
%token <string> STRING

%start tsl_block
%type <Tsl_ast.tsl_block> tsl_block

%%

tsl_tree:
| OPEN tsl_subtree CLOSE { wrap $2 }

tsl_subtree:
| env_item SEMI tsl_subtree {
    let Tsl_node (env, id, mods, subs) = wrap $3 in
    [ Tsl_node ($1 :: env, id, mods, subs) ]
  }
| identifier with_environment_modifiers SEMI tsl_subtree {
    [ Tsl_node ([], $1, $2, $4 ) ]
  }
| tsl_tree_list { $1 }

tsl_tree_list:
| { [] }
| tsl_tree tsl_tree_list { $1 :: $2 }

tsl_block:
| TSL_BEGIN_C_STYLE tsl_script TSL_END_C_STYLE { $2 }
| TSL_BEGIN_OCAML_STYLE tsl_script TSL_END_OCAML_STYLE { $2 }

tsl_script:
| tsl_item tsl_items { Old ($1 :: $2) }
| tsl_tree_list { New $1 }

tsl_items:
| { [] }
| tsl_item tsl_items { $1 :: $2 }

tsl_item:
| test_item { $1 }
| env_item { Environment_statement $1 }

test_item:
  TEST_DEPTH identifier with_environment_modifiers { (Test ($1, $2, $3)) }

with_environment_modifiers:
| { [] }
| WITH identifier opt_environment_modifiers { $2::(List.rev $3) }

opt_environment_modifiers:
| { [] }
| opt_environment_modifiers COMMA identifier { $3::$1 }

env_item:
| identifier EQUAL string
    { mkenvstmt (Assignment (false, $1, $3)) }
| identifier PLUSEQUAL string
    { mkenvstmt (Append ($1, $3)) }
| SET identifier EQUAL string
    { mkenvstmt (Assignment (true, $2, $4)) }
| UNSET identifier
    { mkenvstmt (Unset $2) }

| INCLUDE identifier
  { mkenvstmt (Include $2) }

identifier: IDENTIFIER { mkidentifier $1 }

string: STRING { mkstring $1 }

%%
