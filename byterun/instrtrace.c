/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Trace the instructions executed */

#ifdef DEBUG

#include <stdio.h>
#include <string.h>

#include "instruct.h"
#include "misc.h"
#include "mlvalues.h"
#include "opnames.h"
#include "prims.h"

extern code_t caml_start_code;

long caml_icount = 0;

void caml_stop_here () {}

int caml_trace_flag = 0;

void caml_disasm_instr(pc)
     code_t pc;
{
  int instr = *pc;
  printf("%6ld  %s", (long) (pc - caml_start_code),
         instr < 0 || instr > STOP ? "???" : names_of_instructions[instr]);
  pc++;
  switch(instr) {
    /* Instructions with one integer operand */
  case PUSHACC: case ACC: case POP: case ASSIGN:
  case PUSHENVACC: case ENVACC: case PUSH_RETADDR: case APPLY:
  case APPTERM1: case APPTERM2: case APPTERM3: case RETURN:
  case GRAB: case PUSHGETGLOBAL: case GETGLOBAL: case SETGLOBAL:
  case PUSHATOM: case ATOM: case MAKEBLOCK1: case MAKEBLOCK2:
  case MAKEBLOCK3: case MAKEFLOATBLOCK:
  case GETFIELD: case SETFIELD: case GETFLOATFIELD: case SETFLOATFIELD:
  case BRANCH: case BRANCHIF: case BRANCHIFNOT: case PUSHTRAP:
  case CONSTINT: case PUSHCONSTINT: case OFFSETINT: case OFFSETREF:
  case OFFSETCLOSURE: case PUSHOFFSETCLOSURE:
    printf(" %d\n", pc[0]); break;
    /* Instructions with two operands */
  case APPTERM: case CLOSURE: case CLOSUREREC: case PUSHGETGLOBALFIELD:
  case GETGLOBALFIELD: case MAKEBLOCK:
  case BEQ: case BNEQ: case BLTINT: case BLEINT: case BGTINT: case BGEINT: 
  case BULTINT: case BUGEINT:
    printf(" %d, %d\n", pc[0], pc[1]); break;
    /* Instructions with a C primitive as operand */
  case C_CALLN:
    printf(" %d,", pc[0]); pc++;
    /* fallthrough */
  case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
    if (pc[0] < 0 || pc[0] >= caml_prim_name_table.size)
      printf(" unknown primitive %d\n", pc[0]);
    else
      printf(" %s\n", (char *) caml_prim_name_table.contents[pc[0]]);
    break;
  default:
    printf("\n");
  }
  fflush (stdout);
}




char *
caml_instr_string (code_t pc)
{
  static char buf[96];
  char nambuf[36];
  int instr = *pc;
  char *nam = 0;
  memset (buf, 0, sizeof (buf));
#define bufprintf(Fmt,...) snprintf(buf,sizeof(buf)-1,Fmt,##__VA_ARGS__)
  nam = (instr < 0 || instr > STOP)
    ? (snprintf (nambuf, sizeof (nambuf), "???%d", instr), nambuf)
    : names_of_instructions[instr];
  pc++;
  switch (instr) {
    /* Instructions with one integer operand */
  case PUSHACC:
  case ACC:
  case POP:
  case ASSIGN:
  case PUSHENVACC:
  case ENVACC:
  case PUSH_RETADDR:
  case APPLY:
  case APPTERM1:
  case APPTERM2:
  case APPTERM3:
  case RETURN:
  case GRAB:
  case PUSHGETGLOBAL:
  case GETGLOBAL:
  case SETGLOBAL:
  case PUSHATOM:
  case ATOM:
  case MAKEBLOCK1:
  case MAKEBLOCK2:
  case MAKEBLOCK3:
  case MAKEFLOATBLOCK:
  case GETFIELD:
  case SETFIELD:
  case GETFLOATFIELD:
  case SETFLOATFIELD:
  case BRANCH:
  case BRANCHIF:
  case BRANCHIFNOT:
  case PUSHTRAP:
  case CONSTINT:
  case PUSHCONSTINT:
  case OFFSETINT:
  case OFFSETREF:
  case OFFSETCLOSURE:
  case PUSHOFFSETCLOSURE:
    bufprintf ("%s %d", nam, pc[0]);
    break;
    /* Instructions with two operands */
  case APPTERM:
  case CLOSURE:
  case CLOSUREREC:
  case PUSHGETGLOBALFIELD:
  case GETGLOBALFIELD:
  case MAKEBLOCK:
  case BEQ:
  case BNEQ:
  case BLTINT:
  case BLEINT:
  case BGTINT:
  case BGEINT:
  case BULTINT:
  case BUGEINT:
    bufprintf ("%s %d, %d", nam, pc[0], pc[1]);
    break;
  case SWITCH:
    bufprintf ("SWITCH sz%#lx=%ld::ntag%ld nint%ld",
	       (long) pc[0], (long) pc[0], (unsigned long) pc[0] >> 16,
	       (unsigned long) pc[0] & 0xffff);
    break;
    /* Instructions with a C primitive as operand */
  case C_CALLN:
    bufprintf ("%s %d,", nam, pc[0]);
    pc++;
    /* fallthrough */
  case C_CALL1:
  case C_CALL2:
  case C_CALL3:
  case C_CALL4:
  case C_CALL5:
    if (pc[0] < 0 || pc[0] >= caml_prim_name_table.size)
      bufprintf ("%s unknown primitive %d", nam, pc[0]);
    else
      bufprintf ("%s %s", nam, (char *) caml_prim_name_table.contents[pc[0]]);
    break;
  default:
    bufprintf ("%s", nam);
    break;
  };
  return buf;
}

#endif /* DEBUG */
