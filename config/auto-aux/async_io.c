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

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include "s.h"

int signalled;

void sigio_handler(arg)
     int arg;
{
  signalled = 1;
}

int main()
{
#if defined(SIGIO) && defined(FASYNC) && defined(F_SETFL) && defined(F_SETOWN)
  int p[2];
  int ret;
#define OUT 0
#define IN 1
  if (pipe(p) == -1) return 1;
  signalled = 0;
  signal(SIGIO, sigio_handler);
  ret = fcntl(p[OUT], F_GETFL, 0);
  fcntl(p[OUT], F_SETFL, ret | FASYNC);
  fcntl(p[OUT], F_SETOWN, getpid());
  switch(fork()) {
  case -1:
    return 1;
  case 0:
    close(p[OUT]);
    write(p[IN], "x", 1);
    sleep(1);
    exit(0);
  default:
    close(p[IN]);
    while(wait(NULL) == -1 && errno == EINTR) /*nothing*/;
  }
  if (signalled) return 0; else return 1;
#else
  return 1;
#endif
}
