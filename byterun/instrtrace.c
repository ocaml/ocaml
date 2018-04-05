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

#define CAML_INTERNALS

/* Trace the instructions executed */

#ifdef DEBUG

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "caml/instrtrace.h"
#include "caml/instruct.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/opnames.h"
#include "caml/prims.h"
#include "caml/fiber.h"
#include "caml/domain.h"
#include "caml/startup.h"

extern code_t caml_start_code;

__thread intnat caml_icount = 0;

void caml_stop_here () {}

char * caml_instr_string (code_t pc);

void caml_disasm_instr(code_t pc)
{
  char buf[256];
  char opbuf[128];
  int instr = *pc;
  snprintf(opbuf, sizeof(opbuf), "%6ld  %s", (long) (pc - caml_start_code),
           (instr < 0 || instr >= FIRST_UNIMPLEMENTED_OP) ?
             "???" : names_of_instructions[instr]);
  pc++;
  switch(instr) {
    /* Instructions with one integer operand */
  case PUSHACC: case ACC: case POP: case ASSIGN:
  case PUSHENVACC: case ENVACC: case PUSH_RETADDR: case APPLY:
  case APPTERM1: case APPTERM2: case APPTERM3: case RETURN:
  case GRAB: case PUSHGETGLOBAL: case GETGLOBAL: case SETGLOBAL:
  case PUSHATOM: case ATOM: case MAKEBLOCK1: case MAKEBLOCK2:
  case MAKEBLOCK3: case MAKEFLOATBLOCK:
  case GETFIELD: case GETMUTABLEFIELD: case SETFIELD:
  case GETFLOATFIELD: case SETFLOATFIELD:
  case BRANCH: case BRANCHIF: case BRANCHIFNOT: case PUSHTRAP:
  case CONSTINT: case PUSHCONSTINT: case OFFSETINT: case OFFSETREF:
  case OFFSETCLOSURE: case PUSHOFFSETCLOSURE:
  case RESUMETERM: case REPERFORMTERM:
    snprintf(buf, sizeof(buf), "%s %d\n", opbuf, pc[0]); break;
    /* Instructions with two operands */
  case APPTERM: case CLOSURE: case CLOSUREREC: case PUSHGETGLOBALFIELD:
  case GETGLOBALFIELD: case MAKEBLOCK:
  case BEQ: case BNEQ: case BLTINT: case BLEINT: case BGTINT: case BGEINT:
  case BULTINT: case BUGEINT:
    snprintf(buf, sizeof(buf), "%s %d, %d\n", opbuf, pc[0], pc[1]); break;
    /* Instructions with a C primitive as operand */
  case C_CALLN:
    snprintf(buf, sizeof(buf), "%s %d,", opbuf, pc[0]); pc++;
    /* fallthrough */
  case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
    if (pc[0] < 0 || pc[0] >= caml_prim_name_table.size)
      snprintf(buf, sizeof(buf), "%s unknown primitive %d\n", opbuf, pc[0]);
    else
      snprintf(buf, sizeof(buf), "%s %s\n", opbuf, (char *) caml_prim_name_table.contents[pc[0]]);
    break;
  case SWITCH:
    snprintf(buf, sizeof(buf), "%s ntag=%ld nint=%ld\n",
                 opbuf,
                 (unsigned long) pc[0] >> 16,
                 (unsigned long) pc[0] & 0xffff);
    break;
  default:
    snprintf(buf, sizeof(buf), "%s\n", opbuf);
  }
  printf("[%02d] %s", Caml_state->id, buf);
  fflush (stdout);
}

void
caml_trace_value_file (value v, code_t prog, int proglen, FILE * f)
{
  int i;
  fprintf (f, "%#" ARCH_INTNAT_PRINTF_FORMAT "x", v);
  if (!v)
    return;
  if (prog && v % sizeof (int) == 0
           && (code_t) v >= prog
           && (code_t) v < (code_t) ((char *) prog + proglen))
    fprintf (f, "=code@%ld", Pc_val(v) - prog);
  else if (Is_long (v))
    fprintf (f, "=long%" ARCH_INTNAT_PRINTF_FORMAT "d", Long_val (v));
  else if (caml_on_current_stack((value*)v))
    fprintf (f, "=stack_%ld", (long) ((intnat*)Caml_state->stack_high - (intnat*)v));
  else if (Is_block (v)) {
    int s = Wosize_val (v);
    int tg = Tag_val (v);
    int l = 0;
    switch (tg) {
    case Closure_tag:
      fprintf (f, "=closure[s%d,cod%ld]",
               s, (long) ((code_t) (Code_val (v)) - prog));
      goto displayfields;
    case String_tag:
      l = caml_string_length (v);
      fprintf (f, "=string[s%dL%d]'", s, l);
      for (i = 0; i < ((l>0x1f)?0x1f:l) ; i++) {
        if (isprint ((int) Byte (v, i)))
          putc (Byte (v, i), f);
        else
          putc ('?', f);
      };
      fprintf (f, "'");
      goto displayfields;
    case Double_tag:
      fprintf (f, "=float[s%d]=%g", s, Double_val (v));
      goto displayfields;
    case Double_array_tag:
      fprintf (f, "=floatarray[s%d]", s);
      for (i = 0; i < ((s>0xf)?0xf:s); i++)
        fprintf (f, " %g", Double_flat_field (v, i));
      goto displayfields;
    case Abstract_tag:
      fprintf (f, "=abstract[s%d]", s);
      goto displayfields;
    case Custom_tag:
      fprintf (f, "=custom[s%d]", s);
      goto displayfields;
    default:
      fprintf (f, "=block<T%d/s%d>", tg, s);
    displayfields:
      if (s > 0)
        fputs ("=(", f);
      for (i = 0; i < s; i++) {
        if (i > 20) {
          fputs ("....", f);
          break;
        };
        if (i > 0)
          putc (' ', f);
        fprintf (f, "%#" ARCH_INTNAT_PRINTF_FORMAT "x", Op_val (v)[i]);
      };
      if (s > 0)
        putc (')', f);
    };
  }
}

void
caml_trace_accu_sp_file (value accu, value * sp, code_t prog, int proglen,
                         FILE * f)
{
  int i;
  value *p;
  fprintf (f, "accu=");
  caml_trace_value_file (accu, prog, proglen, f);
  fprintf (f, "\n sp=%#" ARCH_INTNAT_PRINTF_FORMAT "x @%ld:",
           (intnat) sp, (long) (Caml_state->stack_high - sp));
  for (p = sp, i = 0;
       i < 12 + (1 << caml_params->trace_level) && p < Caml_state->stack_high;
       p++, i++) {
    fprintf (f, "\n[%ld] ", (long) (Caml_state->stack_high - p));
    caml_trace_value_file (*p, prog, proglen, f);
  };
  putc ('\n', f);
  fflush (f);
}

#endif /* DEBUG */
