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

typedef void (*scanning_action) (value, value *);

void oldify_local_roots (void);
void darken_all_roots (void);
void do_roots (scanning_action);
#ifndef NATIVE_CODE
void do_local_roots (scanning_action, value *, value *,
                     struct caml__roots_block *);
#else
void do_local_roots(scanning_action f, char * bottom_of_stack,
                    unsigned long last_retaddr, value * gc_regs,
                    struct caml__roots_block * local_roots);
#endif

extern void (*scan_roots_hook) (scanning_action);

#endif /* _roots_ */
