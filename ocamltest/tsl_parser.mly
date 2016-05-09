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

%}

%token TSL_BEGIN TSL_END
%token STAR EQUAL COLON
%token INCLUDE
%token <string> IDENTIFIER
%token <string> STRING

%start tsl_program
%type <Tsl_ast.program> tsl_program

%%

tsl_program:
  TSL_BEGIN statements tests TSL_END
    {
      { Tsl_ast.root_environment = $2; tests = $3 }
    }

statements:
    { [] }
  | statement statements { $1 :: $2 }

tests:
    { [] }
  | test tests { $1 :: $2 }

statement:
    IDENTIFIER EQUAL STRING { Tsl_ast.Assignment ($1, $3) }
  | INCLUDE IDENTIFIER { Tsl_ast.Include $2 }

test:
  STAR IDENTIFIER statements 
    {
      { Tsl_ast.test_name = $2;
        Tsl_ast.test_kind = Tsl_ast.Declared_test;
        test_environemnt = $3 }
    }

%%
