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

#ifdef ARCH_BIG_ENDIAN

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

void ** instr_table;

#if macintosh

void thread_code (code_t code, asize_t len)
{
  code_t p;
  int l [STOP + 1];
  int i;
  
  for (i = 0; i <= STOP; i++){
    l [i] = 0;
  }

	/* Instructions with one operand */
  l[PUSHACC] = l[ACC] = l[POP] = l[ASSIGN] =
  l[PUSHENVACC] = l[ENVACC] = l[PUSH_RETADDR] = l[APPLY] =
  l[APPTERM1] = l[APPTERM2] = l[APPTERM3] = l[RETURN] =
  l[GRAB] = l[PUSHGETGLOBAL] = l[GETGLOBAL] = l[SETGLOBAL] =
  l[PUSHATOM] = l[ATOM] = l[MAKEBLOCK1] = l[MAKEBLOCK2] =
  l[MAKEBLOCK3] = l[GETFIELD] = l[SETFIELD] = l[DUMMY] =
  l[BRANCH] = l[BRANCHIF] = l[BRANCHIFNOT] = l[PUSHTRAP] =
  l[C_CALL1] = l[C_CALL2] = l[C_CALL3] = l[C_CALL4] = l[C_CALL5] =
  l[CONSTINT] = l[PUSHCONSTINT] = l[OFFSETINT] = l[OFFSETREF] = 1;

	/* Instructions with two operands */
  l[APPTERM] = l[CLOSURE] = l[CLOSUREREC] = l[PUSHGETGLOBALFIELD] =
  l[GETGLOBALFIELD] = l[MAKEBLOCK] = l[C_CALLN] = 2;

  len /= sizeof(opcode_t);
  for (p = code; p < code + len; /*nothing*/) {
    opcode_t instr = *p;
	if (instr < 0 || instr > STOP){
	  fatal_error_arg ("Fatal error: bad opcode (%lx)\n", (void *) instr);
	}
    *p++ = (opcode_t)((unsigned long)(instr_table[instr]));
	if (instr == SWITCH){
	  uint32 sizes = *p++;
	  uint32 const_size = sizes & 0xFFFF;
	  uint32 block_size = sizes >> 16;
	  p += const_size + block_size;
	}else{
	  p += l[instr];
    }
  }
  Assert(p == code + len);
}

#else

void thread_code(code, len)
     code_t code;
     asize_t len;
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
  Assert(p == code + len);
}

#endif /* macintosh */

#endif /* THREAD_CODE */
