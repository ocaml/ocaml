/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "m.h"

#if defined(ARCH_INT64_TYPE)
typedef ARCH_INT64_TYPE int64_t;
#elif SIZEOF_LONG == 8
typedef long int64_t;
#elif SIZEOF_LONGLONG == 8
typedef long long int64_t;
#else
#error "No 64-bit integer type available"
#endif

int64_t foo;

void access_int64(int64_t *p)
{
  foo = *p;
}

jmp_buf failure;

void sig_handler(int sig)
{
  longjmp(failure, 1);
}

int main(void)
{
  long n[10];
  int res;
  signal(SIGSEGV, sig_handler);
#ifdef SIGBUS
  signal(SIGBUS, sig_handler);
#endif
  if(setjmp(failure) == 0) {
    access_int64((int64_t *) n);
    access_int64((int64_t *) (n+1));
    res = 0;
  } else {
    res = 1;
  }
  signal(SIGSEGV, SIG_DFL);
#ifdef SIGBUS
  signal(SIGBUS, SIG_DFL);
#endif
  exit(res);
}
