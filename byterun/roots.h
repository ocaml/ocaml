/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#ifndef _roots_
#define _roots_

#include "misc.h"
#include "memory.h"

typedef void (*scanning_action) P((value, value *));

void oldify_local_roots P((void));
void darken_all_roots P((void));
void do_roots P((scanning_action));
#ifndef NATIVE_CODE
void do_local_roots P((scanning_action, value *, value *,
                       struct caml__roots_block *));
#else
void do_local_roots P((scanning_action, unsigned long, char *,
                       struct caml__roots_block *));
#endif

extern void (*scan_roots_hook) P((scanning_action));

#endif /* _roots_ */
