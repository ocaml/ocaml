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

/* Callbacks from C to Caml */

#ifndef _callback_
#define _callback_

#include "mlvalues.h"

value callback (value closure, value arg);
value callback2 (value closure, value arg1, value arg2);
value callback3 (value closure, value arg1, value arg2, value arg3);
extern int callback_depth;

value * caml_named_value (char * name);

void caml_main (char ** argv);
void caml_startup (char ** argv);

#endif
