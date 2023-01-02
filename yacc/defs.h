/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Based on public-domain code from Berkeley Yacc */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define CAML_INTERNALS
#include "caml/osdeps.h"
#include "caml/misc.h"

#define caml_stat_strdup strdup

/*  machine-dependent definitions                              */
/*  the following definitions are for the Tahoe                */
/*  they might have to be changed for other machines           */

/*  MAXCHAR is the largest unsigned character value            */
/*  MAXSHORT is the largest value of a C short                 */
/*  MINSHORT is the most negative value of a C short           */
/*  MAXTABLE is the maximum table size                         */
/*  BITS_PER_INT is the number of bits in a C unsigned         */
/*  WORDSIZE computes the number of words needed to            */
/*        store n bits                                         */
/*  BIT returns the value of the n-th bit starting             */
/*        from r (0-indexed)                                   */
/*  SETBIT sets the n-th bit starting from r                   */

#define        MAXCHAR                UCHAR_MAX
#define        MAXSHORT        SHRT_MAX
#define MINSHORT        SHRT_MIN
#define MAXTABLE        32500

#define BITS_PER_INT    (8*sizeof(unsigned))
#define WORDSIZE(n)     (((n)+(BITS_PER_INT-1))/BITS_PER_INT)
#define BIT(r, n)       ((((r)[(n)/BITS_PER_INT])>>((n)%BITS_PER_INT))&1)
#define SETBIT(r, n)    ((r)[(n)/BITS_PER_INT]|=(1<<((n)%BITS_PER_INT)))

/*  character names  */

#define        NUL                '\0'    /*  the null character  */
#define        NEWLINE                '\n'    /*  line feed  */
#define        SP                ' '     /*  space  */
#define        BS                '\b'    /*  backspace  */
#define        HT                '\t'    /*  horizontal tab  */
#define        VT                '\013'  /*  vertical tab  */
#define        CR                '\r'    /*  carriage return  */
#define        FF                '\f'    /*  form feed  */
#define        QUOTE                '\''    /*  single quote  */
#define        DOUBLE_QUOTE        '\"'    /*  double quote  */
#define        BACKSLASH        '\\'    /*  backslash  */


/* defines for constructing filenames */

#define        OUTPUT_SUFFIX        T(".ml")
#define        VERBOSE_SUFFIX        T(".output")
#define INTERFACE_SUFFIX T(".mli")

/* keyword codes */

#define TOKEN 0
#define LEFT 1
#define RIGHT 2
#define NONASSOC 3
#define MARK 4
#define TEXT 5
#define TYPE 6
#define START 7

/*  symbol classes  */

#define UNKNOWN 0
#define TERM 1
#define NONTERM 2


/*  the undefined value  */

#define UNDEFINED (-1)


/*  action codes  */

#define SHIFT 1
#define REDUCE 2


/*  character macros  */

#define IS_IDENT(c)       (isalnum(c) || (c) == '_' || (c) == '.' || (c) == '$')
#define IS_OCTAL(c)       ((c) >= '0' && (c) <= '7')
#define NUMERIC_VALUE(c)  ((c) - '0')


/*  symbol macros  */

#define ISTOKEN(s)        ((s) < start_symbol)
#define ISVAR(s)        ((s) >= start_symbol)


/*  storage allocation macros  */

#define CALLOC(k,n)      (calloc((unsigned)(k),(unsigned)(n)))
#define FREE(x)          (free((char*)(x)))
#define MALLOC(n)        (malloc((unsigned)(n)))
#define NEW(t)           ((t*)allocate(sizeof(t)))
#define NEW2(n,t)        ((t*)allocate((unsigned)((n)*sizeof(t))))
#define REALLOC(p,n)     (realloc((char*)(p),(unsigned)(n)))


/*  the structure of a symbol table entry  */

typedef struct bucket bucket;
struct bucket
{
    struct bucket *link;
    struct bucket *next;
    char *name;
    char *tag;
    short value;
    short index;
    short prec;
    char class;
    char assoc;
    unsigned char entry;  /* 1..MAX_ENTRY_POINT (0 for unassigned) */
    char true_token;
};

/* MAX_ENTRY_POINT is the maximal number of entry points into the grammar. */
/* Entry points are identified by a non-zero byte in the input stream,     */
/* so there are at most 255 entry points.                                  */

#define MAX_ENTRY_POINT MAXCHAR

/* TABLE_SIZE is the number of entries in the symbol table.      */
/* TABLE_SIZE must be a power of two.                            */

#define        TABLE_SIZE 4096

/*  the structure of the LR(0) state machine  */

typedef struct core core;
struct core
{
    struct core *next;
    struct core *link;
    short number;
    short accessing_symbol;
    short nitems;
    short items[1];
};


/*  the structure used to record shifts  */

typedef struct shifts shifts;
struct shifts
{
    struct shifts *next;
    short number;
    short nshifts;
    short shift[1];
};


/*  the structure used to store reductions  */

typedef struct reductions reductions;
struct reductions
{
    struct reductions *next;
    short number;
    short nreds;
    short rules[1];
};


/*  the structure used to represent parser actions  */

typedef struct action action;
struct action
{
    struct action *next;
    short symbol;
    short number;
    short prec;
    char action_code;
    char assoc;
    char suppressed;
};


/* global variables */

extern char lflag;
extern char rflag;
extern char tflag;
extern char vflag;
extern char qflag;
extern char sflag;
extern char eflag;
extern char big_endian;

/* myname should be UTF-8 encoded */
extern char *myname;
extern char *cptr;
extern char *line;
extern int lineno;
/* virtual_input_file_name should be UTF-8 encoded */
extern char *virtual_input_file_name;
extern int outline;

extern char_os *action_file_name;
extern char_os *entry_file_name;
extern char_os *code_file_name;
extern char_os *input_file_name;
extern char_os *output_file_name;
extern char_os *text_file_name;
extern char_os *verbose_file_name;
extern char_os *interface_file_name;

/* UTF-8 versions of code_file_name and input_file_name */
extern char *code_file_name_disp;
extern char *input_file_name_disp;

extern FILE *action_file;
extern FILE *entry_file;
extern FILE *code_file;
extern FILE *input_file;
extern FILE *output_file;
extern FILE *text_file;
extern FILE *verbose_file;
extern FILE *interface_file;

extern int nitems;
extern int nrules;
extern int ntotalrules;
extern int nsyms;
extern int ntokens;
extern int nvars;
extern int ntags;

#define line_format "# %d \"%s\"\n"

extern int   start_symbol;
extern char  **symbol_name;
extern short *symbol_value;
extern short *symbol_prec;
extern char  *symbol_assoc;
extern char  **symbol_tag;
extern char  *symbol_true_token;

extern short *ritem;
extern short *rlhs;
extern short *rrhs;
extern short *rprec;
extern char  *rassoc;

extern short **derives;
extern char *nullable;

extern bucket *first_symbol;
extern bucket *last_symbol;

extern int nstates;
extern core *first_state;
extern shifts *first_shift;
extern reductions *first_reduction;
extern short *accessing_symbol;
extern core **state_table;
extern shifts **shift_table;
extern reductions **reduction_table;
extern unsigned *LA;
extern short *LAruleno;
extern short *lookaheads;
extern short *goto_map;
extern short *from_state;
extern short *to_state;

extern action **parser;
extern int SRtotal;
extern int RRtotal;
extern short *SRconflicts;
extern short *RRconflicts;
extern short *defred;
extern short *rules_used;
extern short nunused;
extern short final_state;

/* global functions */

extern char *allocate(unsigned int n);
extern bucket *lookup(char *name);
extern bucket *make_bucket(char *name);
extern action *parse_actions(register int stateno);
extern action *get_shifts(int stateno);
extern action *add_reductions(int stateno, register action *actions);
extern action *add_reduce(register action *actions, register int ruleno, register int symbol);
extern void closure (short int *nucleus, int n);
extern void create_symbol_table (void);
extern void default_action_error (void) Noreturn;
extern void done (int k) Noreturn;
extern void entry_without_type (char *s) Noreturn;
extern void fatal (char *msg) Noreturn;
extern void finalize_closure (void);
extern void free_parser (void);
extern void free_symbol_table (void);
extern void free_symbols (void);
extern void illegal_character (char *c_cptr) Noreturn;
extern void illegal_token_ref (int i, char *name) Noreturn;
extern void lalr (void);
extern void lr0 (void);
extern void make_parser (void);
extern void no_grammar (void) Noreturn;
extern void no_space (void) Noreturn;
extern void open_error (char_os *filename) Noreturn;
extern void output (void);
extern void prec_redeclared (void);
extern void polymorphic_entry_point(char *s) Noreturn;
extern void forbidden_conflicts (void);
extern void reader (void);
extern void reflexive_transitive_closure (unsigned int *R, int n);
extern void reprec_warning (char *s);
extern void retyped_warning (char *s);
extern void revalued_warning (char *s);
extern void set_first_derives (void);
extern void syntax_error (int st_lineno, char *st_line, char *st_cptr) Noreturn, terminal_lhs (int s_lineno) Noreturn;
extern void terminal_start (char *s) Noreturn;
extern void tokenized_start (char *s) Noreturn;
extern void too_many_entries (void) Noreturn;
extern void undefined_goal (char *s);
extern void undefined_symbol (char *s);
extern void unexpected_EOF (void) Noreturn;
extern void unknown_rhs (int i) Noreturn;
extern void unterminated_action (int a_lineno, char *a_line, char *a_cptr) Noreturn;
extern void unterminated_comment (int c_lineno, char *c_line, char *c_cptr) Noreturn;
extern void unterminated_string (int s_lineno, char *s_line, char *s_cptr) Noreturn;
extern void unterminated_text (int t_lineno, char *t_line, char *t_cptr) Noreturn;
extern void used_reserved (char *s) Noreturn;
extern void verbose (void);
extern void write_section (char **section);
