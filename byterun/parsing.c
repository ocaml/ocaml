/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* The PDA automaton for parsers generated by camlyacc */

#include <stdio.h>
#include <string.h>
#include "config.h"
#include "mlvalues.h"
#include "memory.h"
#include "alloc.h"

#define ERRCODE 256

struct parser_tables {    /* Mirrors parse_tables in ../stdlib/parsing.mli */
  value actions;
  value transl_const;
  value transl_block;
  char * lhs;
  char * len;
  char * defred;
  char * dgoto;
  char * sindex;
  char * rindex;
  char * gindex;
  value tablesize;
  char * table;
  char * check;
  value error_function;
  char * names_const;
  char * names_block;
};

struct parser_env {       /* Mirrors parser_env in ../stdlib/parsing.ml */
  value s_stack;
  value v_stack;
  value symb_start_stack;
  value symb_end_stack;
  value stacksize;
  value stackbase;
  value curr_char;
  value lval;
  value symb_start;
  value symb_end;
  value asp;
  value rule_len;
  value rule_number;
  value sp;
  value state;
  value errflag;
};

#if defined(ARCH_BIG_ENDIAN) || SIZEOF_SHORT != 2
#define Short(tbl,n) \
  (*((unsigned char *)((tbl) + (n) * 2)) + \
          (*((signed char *)((tbl) + (n) * 2 + 1)) << 8))
#else
#define Short(tbl,n) (((short *)(tbl))[n])
#endif

int caml_parser_trace = 0;

/* Input codes */
/* Mirrors parser_input in ../stdlib/parsing.ml */
#define START 0
#define TOKEN_READ 1
#define STACKS_GROWN_1 2
#define STACKS_GROWN_2 3
#define SEMANTIC_ACTION_COMPUTED 4
#define ERROR_DETECTED 5

/* Output codes */
/* Mirrors parser_output in ../stdlib/parsing.ml */
#define READ_TOKEN Val_int(0)
#define RAISE_PARSE_ERROR Val_int(1)
#define GROW_STACKS_1 Val_int(2)
#define GROW_STACKS_2 Val_int(3)
#define COMPUTE_SEMANTIC_ACTION Val_int(4)
#define CALL_ERROR_FUNCTION Val_int(5)

/* To preserve local variables when communicating with the ML code */

#define SAVE \
  env->sp = Val_int(sp), \
  env->state = Val_int(state), \
  env->errflag = Val_int(errflag)

#define RESTORE \
  sp = Int_val(env->sp), \
  state = Int_val(env->state), \
  errflag = Int_val(env->errflag)

/* Auxiliary for printing token just read */

static char * token_name(char * names, int number)
{
  for (/*nothing*/; number > 0; number--) {
    if (names[0] == 0) return "<unknown token>";
    names += strlen(names) + 1;
  }
  return names;
}

static void print_token(struct parser_tables *tables, int state, value tok)
{
  value v;

  if (Is_long(tok)) {
    fprintf(stderr, "State %d: read token %s\n",
            state, token_name(tables->names_const, Int_val(tok)));
  } else {
    fprintf(stderr, "State %d: read token %s(",
            state, token_name(tables->names_block, Tag_val(tok)));
    v = Field(tok, 0);
    if (Is_long(v))
      fprintf(stderr, "%" ARCH_INTNAT_PRINTF_FORMAT "d", Long_val(v));
    else if (Tag_val(v) == String_tag)
      fprintf(stderr, "%s", String_val(v));
    else if (Tag_val(v) == Double_tag)
      fprintf(stderr, "%g", Double_val(v));
    else
      fprintf(stderr, "_");
    fprintf(stderr, ")\n");
  }
}

/* The pushdown automata */

CAMLprim value caml_parse_engine(struct parser_tables *tables,
                                 struct parser_env *env, value cmd, value arg)
{
  int state;
  mlsize_t sp, asp;
  int errflag;
  int n, n1, n2, m, state1;

  switch(Int_val(cmd)) {

  case START:
    state = 0;
    sp = Int_val(env->sp);
    errflag = 0;

  loop:
    n = Short(tables->defred, state);
    if (n != 0) goto reduce;
    if (Int_val(env->curr_char) >= 0) goto testshift;
    SAVE;
    return READ_TOKEN;
                                /* The ML code calls the lexer and updates */
                                /* symb_start and symb_end */
  case TOKEN_READ:
    RESTORE;
    if (Is_block(arg)) {
      env->curr_char = Field(tables->transl_block, Tag_val(arg));
      caml_modify(&env->lval, Field(arg, 0));
    } else {
      env->curr_char = Field(tables->transl_const, Int_val(arg));
      caml_modify(&env->lval, Val_long(0));
    }
    if (caml_parser_trace) print_token(tables, state, arg);

  testshift:
    n1 = Short(tables->sindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) goto shift;
    n1 = Short(tables->rindex, state);
    n2 = n1 + Int_val(env->curr_char);
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == Int_val(env->curr_char)) {
      n = Short(tables->table, n2);
      goto reduce;
    }
    if (errflag > 0) goto recover;
    SAVE;
    return CALL_ERROR_FUNCTION;
                                /* The ML code calls the error function */
  case ERROR_DETECTED:
    RESTORE;
  recover:
    if (errflag < 3) {
      errflag = 3;
      while (1) {
        state1 = Int_val(Field(env->s_stack, sp));
        n1 = Short(tables->sindex, state1);
        n2 = n1 + ERRCODE;
        if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
            Short(tables->check, n2) == ERRCODE) {
          if (caml_parser_trace)
            fprintf(stderr, "Recovering in state %d\n", state1);
          goto shift_recover;
        } else {
          if (caml_parser_trace){
            fprintf(stderr, "Discarding state %d\n", state1);
          }
          if (sp <= Int_val(env->stackbase)) {
            if (caml_parser_trace){
              fprintf(stderr, "No more states to discard\n");
            }
            return RAISE_PARSE_ERROR; /* The ML code raises Parse_error */
          }
          sp--;
        }
      }
    } else {
      if (Int_val(env->curr_char) == 0)
        return RAISE_PARSE_ERROR; /* The ML code raises Parse_error */
      if (caml_parser_trace) fprintf(stderr, "Discarding last token read\n");
      env->curr_char = Val_int(-1);
      goto loop;
    }

  shift:
    env->curr_char = Val_int(-1);
    if (errflag > 0) errflag--;
  shift_recover:
    if (caml_parser_trace)
      fprintf(stderr, "State %d: shift to state %d\n",
              state, Short(tables->table, n2));
    state = Short(tables->table, n2);
    sp++;
    if (sp < Long_val(env->stacksize)) goto push;
    SAVE;
    return GROW_STACKS_1;
                                 /* The ML code resizes the stacks */
  case STACKS_GROWN_1:
    RESTORE;
  push:
    Field(env->s_stack, sp) = Val_int(state);
    caml_modify_field(env->v_stack, sp, env->lval);
    Store_field (env->symb_start_stack, sp, env->symb_start);
    Store_field (env->symb_end_stack, sp, env->symb_end);
    goto loop;

  reduce:
    if (caml_parser_trace)
      fprintf(stderr, "State %d: reduce by rule %d\n", state, n);
    m = Short(tables->len, n);
    env->asp = Val_int(sp);
    env->rule_number = Val_int(n);
    env->rule_len = Val_int(m);
    sp = sp - m + 1;
    m = Short(tables->lhs, n);
    state1 = Int_val(Field(env->s_stack, sp - 1));
    n1 = Short(tables->gindex, m);
    n2 = n1 + state1;
    if (n1 != 0 && n2 >= 0 && n2 <= Int_val(tables->tablesize) &&
        Short(tables->check, n2) == state1) {
      state = Short(tables->table, n2);
    } else {
      state = Short(tables->dgoto, m);
    }
    if (sp < Long_val(env->stacksize)) goto semantic_action;
    SAVE;
    return GROW_STACKS_2;
                                /* The ML code resizes the stacks */
  case STACKS_GROWN_2:
    RESTORE;
  semantic_action:
    SAVE;
    return COMPUTE_SEMANTIC_ACTION;
                                /* The ML code calls the semantic action */
  case SEMANTIC_ACTION_COMPUTED:
    RESTORE;
    Field(env->s_stack, sp) = Val_int(state);
    caml_modify_field(env->v_stack, sp, arg);
    asp = Int_val(env->asp);
    Store_field (env->symb_end_stack, sp, Field(env->symb_end_stack, asp));
    if (sp > asp) {
      /* This is an epsilon production. Take symb_start equal to symb_end. */
      Store_field (env->symb_start_stack, sp, Field(env->symb_end_stack, asp));
    }
    goto loop;

  default:                      /* Should not happen */
    Assert(0);
    return RAISE_PARSE_ERROR;   /* Keeps gcc -Wall happy */
  }

}

/* Control printing of debugging info */

CAMLprim value caml_set_parser_trace(value flag)
{
  value oldflag = Val_bool(caml_parser_trace);
  caml_parser_trace = Bool_val(flag);
  return oldflag;
}
