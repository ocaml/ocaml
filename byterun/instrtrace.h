/***********************************************************************/
/*                                                                     */
/*                         Caml Special Light                          */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1995 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Trace the instructions executed */

#ifndef _instrtrace_
#define _instrtrace_


#include "mlvalues.h"
#include "misc.h"

extern int trace_flag;
extern long icount;
void stop_here P((void));
void disasm_instr P((code_t pc));


#endif
