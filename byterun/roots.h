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

typedef void (*scanning_action) P((value, value *));

void oldify_local_roots P((void));
void darken_all_roots P((void));
void do_roots P((scanning_action));

extern void (*scan_roots_hook) P((scanning_action));

#endif /* _roots_ */
