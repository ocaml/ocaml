/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* Based on public-domain code from Berkeley Yacc */

/* $Id$ */

/* routines for printing error messages  */

#include "defs.h"

void fatal(char *msg)
{
    fprintf(stderr, "%s: f - %s\n", myname, msg);
    done(2);
}


void no_space(void)
{
    fprintf(stderr, "%s: f - out of space\n", myname);
    done(2);
}


void open_error(char *filename)
{
    fprintf(stderr, "%s: f - cannot open \"%s\"\n", myname, filename);
    done(2);
}


void unexpected_EOF(void)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unexpected end-of-file\n",
            myname, lineno, virtual_input_file_name);
    done(1);
}


void print_pos(char *st_line, char *st_cptr)
{
    register char *s;

    if (st_line == 0) return;
    for (s = st_line; *s != '\n'; ++s)
    {
        if (isprint((unsigned char) *s) || *s == '\t')
            putc(*s, stderr);
        else
            putc('?', stderr);
    }
    putc('\n', stderr);
    for (s = st_line; s < st_cptr; ++s)
    {
        if (*s == '\t')
            putc('\t', stderr);
        else
            putc(' ', stderr);
    }
    putc('^', stderr);
    putc('\n', stderr);
}


void syntax_error(int st_lineno, char *st_line, char *st_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", syntax error\n",
            myname, st_lineno, virtual_input_file_name);
    print_pos(st_line, st_cptr);
    done(1);
}


void unterminated_comment(int c_lineno, char *c_line, char *c_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unmatched /*\n",
            myname, c_lineno, virtual_input_file_name);
    print_pos(c_line, c_cptr);
    done(1);
}


void unterminated_string(int s_lineno, char *s_line, char *s_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unterminated string\n",
            myname, s_lineno, virtual_input_file_name);
    print_pos(s_line, s_cptr);
    done(1);
}


void unterminated_text(int t_lineno, char *t_line, char *t_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unmatched %%{\n",
            myname, t_lineno, virtual_input_file_name);
    print_pos(t_line, t_cptr);
    done(1);
}


void unterminated_union(int u_lineno, char *u_line, char *u_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unterminated %%union \
declaration\n", myname, u_lineno, virtual_input_file_name);
    print_pos(u_line, u_cptr);
    done(1);
}


void over_unionized(char *u_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", too many %%union \
declarations\n", myname, lineno, virtual_input_file_name);
    print_pos(line, u_cptr);
    done(1);
}


void illegal_tag(int t_lineno, char *t_line, char *t_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", illegal tag\n",
            myname, t_lineno, virtual_input_file_name);
    print_pos(t_line, t_cptr);
    done(1);
}


void illegal_character(char *c_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", illegal character\n",
            myname, lineno, virtual_input_file_name);
    print_pos(line, c_cptr);
    done(1);
}


void used_reserved(char *s)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", illegal use of reserved symbol \
%s\n", myname, lineno, virtual_input_file_name, s);
    done(1);
}


void tokenized_start(char *s)
{
     fprintf(stderr, "%s: e - line %d of \"%s\", the start symbol %s cannot be \
declared to be a token\n", myname, lineno, virtual_input_file_name, s);
     done(1);
}


void retyped_warning(char *s)
{
    fprintf(stderr, "%s: w - line %d of \"%s\", the type of %s has been \
redeclared\n", myname, lineno, virtual_input_file_name, s);
}


void reprec_warning(char *s)
{
    fprintf(stderr, "%s: w - line %d of \"%s\", the precedence of %s has been \
redeclared\n", myname, lineno, virtual_input_file_name, s);
}


void revalued_warning(char *s)
{
    fprintf(stderr, "%s: w - line %d of \"%s\", the value of %s has been \
redeclared\n", myname, lineno, virtual_input_file_name, s);
}


void terminal_start(char *s)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", the entry point %s is a \
token\n", myname, lineno, virtual_input_file_name, s);
    done(1);
}

void too_many_entries(void)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", more than 256 entry points\n",
            myname, lineno, virtual_input_file_name);
    done(1);
}


void no_grammar(void)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", no grammar has been \
specified\n", myname, lineno, virtual_input_file_name);
    done(1);
}


void terminal_lhs(int s_lineno)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", a token appears on the lhs \
of a production\n", myname, s_lineno, virtual_input_file_name);
    done(1);
}


void prec_redeclared(void)
{
    fprintf(stderr, "%s: w - line %d of  \"%s\", conflicting %%prec \
specifiers\n", myname, lineno, virtual_input_file_name);
}


void unterminated_action(int a_lineno, char *a_line, char *a_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", unterminated action\n",
            myname, a_lineno, virtual_input_file_name);
    print_pos(a_line, a_cptr);
    done(1);
}


void dollar_warning(int a_lineno, int i)
{
    fprintf(stderr, "%s: w - line %d of \"%s\", $%d references beyond the \
end of the current rule\n", myname, a_lineno, virtual_input_file_name, i);
}


void dollar_error(int a_lineno, char *a_line, char *a_cptr)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", illegal $-name\n",
            myname, a_lineno, virtual_input_file_name);
    print_pos(a_line, a_cptr);
    done(1);
}


void untyped_lhs(void)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", $$ is untyped\n",
            myname, lineno, virtual_input_file_name);
    done(1);
}


void untyped_rhs(int i, char *s)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", $%d (%s) is untyped\n",
            myname, lineno, virtual_input_file_name, i, s);
    done(1);
}


void unknown_rhs(int i)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", $%d is unbound\n",
            myname, lineno, virtual_input_file_name, i);
    done(1);
}

void illegal_token_ref(int i, char *name)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", $%d refers to terminal `%s', which has no argument\n",
            myname, lineno, virtual_input_file_name, i, name);
    done(1);
}

void default_action_error(void)
{
    fprintf(stderr, "%s: e - line %d of \"%s\", no action specified for this production\n",
            myname, lineno, virtual_input_file_name);
    done(1);
}


void undefined_goal(char *s)
{
    fprintf(stderr, "%s: e - the start symbol %s is undefined\n", myname, s);
    done(1);
}

void undefined_symbol(char *s)
{
    fprintf(stderr, "%s: e - the symbol %s is undefined\n", myname, s);
    done(1);
}


void entry_without_type(char *s)
{
    fprintf(stderr,
            "%s: e - no type has been declared for the start symbol %s\n",
            myname, s);
    done(1);
}

void polymorphic_entry_point(char *s)
{
    fprintf(stderr,
            "%s: e - the start symbol %s has a polymorphic type\n",
            myname, s);
    done(1);
}

