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

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include "unix.h"

value unix_wait()                /* ML */
{
  value res;
  int pid, status;
  Push_roots(r, 1);
#define st r[0]
  pid = wait(&status);
  if (pid == -1) uerror("wait", Nothing);
  switch (status & 0xFF) {
  case 0:
    st = alloc(1, 0);
    Field(st, 0) = Val_int((status >> 8) & 0xFF);
    break;
  case 0177:
    st = alloc(1, 2);
    Field(st, 0) = Val_int((status >> 8) & 0xFF);
    break;
  default:
    st = alloc(2, 1);
    Field(st, 0) = Val_int(status & 0x3F);
    Field(st, 1) = status & 0200 ? Val_true : Val_false;
    break;
  }
  res = alloc_tuple(2);
  Field(res, 0) = Val_int(pid);
  Field(res, 1) = st;
  Pop_roots();
  return res;
}

