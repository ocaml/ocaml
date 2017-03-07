/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <caml/mlvalues.h>

void print_string(char * s)
{
  fputs(s, stdout);
}

void printf_int(char * fmt, int arg)
{
  printf(fmt, arg);
}

#ifdef SORT

int cmpint(const void * i, const void * j)
{
  long vi = *((long *) i);
  long vj = *((long *) j);
  if (vi == vj) return 0;
  if (vi < vj) return -1;
  return 1;
}

#endif

CAMLprim value run_prog(value varg1, value varg2, value varg3)
{
  long arg1 = Long_val(varg1);
  long arg2 = Long_val(varg2);
  long arg3 = Long_val(varg3);
  if (arg1+arg2+arg3); /* squash unused var warnings */
#ifdef UNIT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN));
  }
#else
#ifdef INT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN, arg1));
  }
#endif
#ifdef INT_FLOAT
  { extern double FUN();
#ifdef __mc68020__
#define call_gen_code call_gen_code_float
#endif
    extern double call_gen_code();
    printf("%f\n", call_gen_code(FUN, arg1));
  }
#endif
#ifdef SORT
  { extern void FUN();
    extern void call_gen_code();
    long n;
    long * a, * b;
    long i;

    srand(arg2 ? arg2 : time(0));
    n = arg1;
    a = (long *) malloc(n * sizeof(long));
    for (i = 0 ; i < n; i++) a[i] = rand() & 0xFFF;
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%ld ", a[i]); printf("\n");
#endif
    b = (long *) malloc(n * sizeof(long));
    for (i = 0; i < n; i++) b[i] = a[i];
    call_gen_code(FUN, 0, n-1, a);
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%ld ", a[i]); printf("\n");
#endif
    qsort(b, n, sizeof(long), cmpint);
    for (i = 0; i < n; i++) {
      if (a[i] != b[i]) { printf("Bug!\n"); return 2; }
    }
    printf("OK\n");
  }
#endif
#endif
#ifdef CHECKBOUND
  { extern void checkbound1(), checkbound2();
    extern void call_gen_code();
    long x, y;
    x = arg1;
    if (arg2) {
      y = arg2;
      if ((unsigned long) x < (unsigned long) y)
        printf("Should not trap\n");
      else
        printf("Should trap\n");
      call_gen_code(checkbound2, y, x);
    } else {
      if (2 < (unsigned long) x)
        printf("Should not trap\n");
      else
        printf("Should trap\n");
      call_gen_code(checkbound1, x);
    }
    printf("OK\n");
  }
#endif
  return Val_unit;
}
