/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
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
void stop_here (void);
void disasm_instr (code_t pc);


#endif
