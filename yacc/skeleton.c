/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* Based on public-domain code from Berkeley Yacc */

/* $Id$ */

#include "defs.h"

char *header[] =
{
  "open Parsing",
  0
};

char *define_tables[] =
{
  "let yytables =",
  "  { actions=yyact;",
  "    transl_const=yytransl_const;",
  "    transl_block=yytransl_block;",
  "    lhs=yylhs;",
  "    len=yylen;",
  "    defred=yydefred;",
  "    dgoto=yydgoto;",
  "    sindex=yysindex;",
  "    rindex=yyrindex;",
  "    gindex=yygindex;",
  "    tablesize=yytablesize;",
  "    table=yytable;",
  "    check=yycheck;",
  "    error_function=parse_error }",
  0
};

void write_section(section)
        char **section;
{
    register int i;
    register FILE *fp;

    fp = code_file;
    for (i = 0; section[i]; ++i)
    {
	++outline;
	fprintf(fp, "%s\n", section[i]);
    }
}
