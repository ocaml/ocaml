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

/* $Id$ */

/* Translate a block of bytecode (endianness switch, threading). */

#include "config.h"
#include "fix_code.h"
#include "misc.h"
#include "mlvalues.h"
#include "instruct.h"
#include "reverse.h"

/* This code is needed only if the processor is big endian */

#ifdef BIG_ENDIAN

void fixup_endianness(code, len)
     code_t code;
     asize_t len;
{
  code_t p;
  len /= sizeof(opcode_t);
  for (p = code; p < code + len; p++) {
    Reverse_int32(p);
  }
}

#endif

/* This code is needed only if we're using threaded code */

#ifdef THREADED_CODE

void thread_code(code, len, instr_table)
     code_t code;
     asize_t len;
     void * instr_table[];
{
  code_t p;
  len /= sizeof(opcode_t);
  for (p = code; p < code + len; /*nothing*/) {
    opcode_t instr = *p;
    Assert(instr >= 0 && instr <= STOP);
    *p++ = (opcode_t)((unsigned long)(instr_table[instr]));
    switch(instr) {
      /* Instructions with one operand */
    case PUSHACC: case ACC: case POP: case ASSIGN:
    case PUSHENVACC: case ENVACC: case PUSH_RETADDR: case APPLY:
    case APPTERM1: case APPTERM2: case APPTERM3: case RETURN:
    case GRAB: case PUSHGETGLOBAL: case GETGLOBAL: case SETGLOBAL:
    case PUSHATOM: case ATOM: case MAKEBLOCK1: case MAKEBLOCK2:
    case MAKEBLOCK3: case GETFIELD: case SETFIELD: case DUMMY:
    case BRANCH: case BRANCHIF: case BRANCHIFNOT: case PUSHTRAP:
    case C_CALL1: case C_CALL2: case C_CALL3: case C_CALL4: case C_CALL5:
    case CONSTINT: case PUSHCONSTINT: case OFFSETINT: case OFFSETREF:
      p += 1; break;
      /* Instructions with two operands */
    case APPTERM: case CLOSURE: case CLOSUREREC: case PUSHGETGLOBALFIELD:
    case GETGLOBALFIELD: case MAKEBLOCK: case C_CALLN:
      p += 2; break;
      /* Instructions with N+1 operands */
    case SWITCH:
      { uint32 sizes = *p++;
        uint32 const_size = sizes & 0xFFFF;
        uint32 block_size = sizes >> 16;
        p += const_size + block_size;
        break; }
    }
  }
  Assert(p = code + len);
}

#endif
