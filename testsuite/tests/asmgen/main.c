/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "caml/config.h"

void caml_ml_array_bound_error(void)
{
  fprintf(stderr, "Fatal error: out-of-bound access in array or string\n");
  exit(2);
}

void print_string(char * s)
{
  fputs(s, stdout);
}

void printf_int(char * fmt, intnat arg)
{
  printf(fmt, arg);
}

#define FLOATTEST(arg,res) \
  { double result = (res); \
    if (arg < result || arg > result) { \
      printf("Failed test \"%s == %s\": " \
             "result %.15g, expected %.15g\n", \
             #arg, #res, arg, result); \
      return(2); \
    } \
  }

#ifdef SORT

int cmpint(const void * i, const void * j)
{
  intnat vi = *((intnat *) i);
  intnat vj = *((intnat *) j);
  if (vi == vj) return 0;
  if (vi < vj) return -1;
  return 1;
}

#endif

int main(int argc, char **argv)
{
#ifdef UNIT_INT
  { extern intnat FUN(void);
    extern intnat call_gen_code(intnat (*)(void));
    printf("%"ARCH_INTNAT_PRINTF_FORMAT"d\n", call_gen_code(FUN));
  }
#else
  if (argc < 2) {
    fprintf(stderr, "Usage: %s [int arg]\n", argv[0]);
    exit(2);
  }
#ifdef INT_INT
  { extern intnat FUN(intnat);
    extern intnat call_gen_code(intnat (*)(intnat), intnat);
    printf("%"ARCH_INTNAT_PRINTF_FORMAT"d\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef INT_FLOAT
  { extern double FUN(intnat);
    extern double call_gen_code(double (*)(intnat), intnat);
    printf("%f\n", call_gen_code(FUN, atoi(argv[1])));
  }
#endif
#ifdef FLOAT_CATCH
  { extern double FUN(intnat);
    extern double call_gen_code(double (*)(intnat), intnat);
    double result = call_gen_code(FUN, 1);
    FLOATTEST(result, 1110.0)
    printf("%f\n", result);
  }
#endif
#ifdef SORT
  { extern void FUN(intnat, intnat, intnat *);
    extern void call_gen_code(void (*)(intnat, intnat, intnat *), intnat, intnat, intnat *);
    intnat n;
    intnat * a, * b;
    intnat i;

    srand(argc >= 3 ? atoi(argv[2]) : time((time_t *) 0));
    n = atoi(argv[1]);
    a = (intnat *) malloc(n * sizeof(intnat));
    for (i = 0 ; i < n; i++) a[i] = rand() & 0xFFF;
#ifdef DEBUG
    for (i = 0; i < n; i++)
      printf("%"ARCH_INTNAT_PRINTF_FORMAT"d ", a[i]);
    printf("\n");
#endif
    b = (intnat *) malloc(n * sizeof(intnat));
    for (i = 0; i < n; i++) b[i] = a[i];
    call_gen_code(FUN, 0, n-1, a);
#ifdef DEBUG
    for (i = 0; i < n; i++)
      printf("%"ARCH_INTNAT_PRINTF_FORMAT"d ", a[i]);
    printf("\n");
#endif
    qsort(b, n, sizeof(intnat), cmpint);
    for (i = 0; i < n; i++) {
      if (a[i] != b[i]) { printf("Bug!\n"); return 2; }
    }
    printf("OK\n");
  }
#endif
#endif
#ifdef CHECKBOUND
  { extern void checkbound1(intnat), checkbound2(intnat, intnat);
    extern void call_gen_code(void *, ...);
    intnat x, y;
    x = atoi(argv[1]);
    if (argc >= 3) {
      y = atoi(argv[2]);
      if ((uintnat) x < (uintnat) y)
        printf("Should not trap\n");
      else
        printf("Should trap\n");
      call_gen_code(checkbound2, y, x);
    } else {
      if (2 < (uintnat) x)
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
