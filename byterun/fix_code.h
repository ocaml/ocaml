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

/* Handling of blocks of bytecode (endianness switch, threading). */

#ifndef _fix_code_
#define _fix_code_


#include "config.h"
#include "misc.h"
#include "mlvalues.h"

extern code_t start_code;
extern asize_t code_size;
extern unsigned char * saved_code;

void load_code P((int fd, asize_t len));
void fixup_endianness P((code_t code, asize_t len));
void set_instruction P((code_t pos, opcode_t instr));

#ifdef THREADED_CODE
extern char ** instr_table;
extern char * instr_base;
void thread_code P((code_t code, asize_t len));
#endif

#endif
