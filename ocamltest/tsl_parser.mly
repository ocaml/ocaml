/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Sébastien Hinderer, projet Gallium, INRIA Paris           */
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
  let located_env_statement =
    make_environment_statement ~loc:(symbol_rloc()) envstmt in
  Environment_statement located_env_statement

%}

%token TSL_BEGIN TSL_END
%token <int> TEST_DEPTH
%token EQUAL
/* %token COLON */
%token INCLUDE
%token <string> IDENTIFIER
%token <string> STRING

%start tsl_block
%type <Tsl_ast.tsl_block> tsl_block

%%

tsl_block:
  TSL_BEGIN tsl_items TSL_END { $2 }

tsl_items:
| { [] }
| tsl_item tsl_items { $1 :: $2 }

tsl_item:
| test_item { $1 }
| env_item { $1 }

test_item:
  TEST_DEPTH identifier { (Test ($1, $2)) }

env_item:
| identifier EQUAL string
  { mkenvstmt (Assignment ($1, $3)) }
| INCLUDE identifier
  { mkenvstmt (Include $2) }

identifier: IDENTIFIER { mkidentifier $1 }

string: STRING { mkstring $1 }

%%
