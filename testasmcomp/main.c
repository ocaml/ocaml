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

#ifdef SORT

long cmpint(i, j)
     long * i, * j;
{
  return *i - *j;
}

#endif

int main(argc, argv)
     int argc;
     char ** argv;
{
#ifdef UNIT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN));
  }
#else
  if (argc < 2) {
    fprintf(stderr, "Usage: %s [int arg]\n", argv[0]);
    exit(2);
  }
#ifdef INT_INT
  { extern int FUN();
    extern int call_gen_code();
    printf("%d\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef INT_FLOAT
  { extern double FUN();
    extern double call_gen_code();
    printf("%f\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef SORT
  { extern void FUN();
    extern void call_gen_code();
    long n;
    long * a, * b;
    long i;

    srand(argc >= 3 ? atoi(argv[2]) : time((char *) 0));
    n = atoi(argv[1]);
    a = (long *) malloc(n * sizeof(long));
    for (i = 0 ; i < n; i++) a[i] = rand() & 0xFFF;
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%d ", a[i]); printf("\n");
#endif
    b = (long *) malloc(n * sizeof(long));
    for (i = 0; i < n; i++) b[i] = a[i];
    call_gen_code(FUN, 0, n-1, a);
#ifdef DEBUG
    for (i = 0; i < n; i++) printf("%d ", a[i]); printf("\n");
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
    x = atoi(argv[1]);
    if (argc >= 3) {
      y = atoi(argv[2]);
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
  return 0;
}
