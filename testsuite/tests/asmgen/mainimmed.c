#include <stdio.h>
#include <stdlib.h>
#include <caml/config.h>

#define NUMTESTS 37
intnat R[NUMTESTS][7];
intnat X;

extern void call_gen_code(void (*)(void));
extern void testimm(void);

void caml_ml_array_bound_error(void)
{
  fprintf(stderr, "Fatal error: out-of-bound access in array or string\n");
  exit(2);
}

/* One round of testing */

#define FMT ARCH_INTNAT_PRINTF_FORMAT

static void check(int i, intnat x, intnat result, intnat expected)
{
  if (result != expected) {
    printf("Test %d, argument %"FMT"d: got %"FMT"d, expected %"FMT"d\n",
           i, x, result, expected);
  }
}

static void test_one(int i, intnat x, intnat y)
{
  check(i, x, R[i][0], x + y);
  check(i, x, R[i][1], x - y);
  check(i, x, R[i][2], x * y);
  check(i, x, R[i][3], x & y);
  check(i, x, R[i][4], x | y);
  check(i, x, R[i][5], x ^ y);
  check(i, x, R[i][6], x < y);
}

static void do_test(intnat x)
{
  int i;

  X = x;
  call_gen_code(testimm);
  i = 0;
#define F(N) test_one(i++, x, N);
#include "immediates.tbl"
}

/* A simple linear congruential PRNG */

#ifdef ARCH_SIXTYFOUR
#define RAND_A 6364136223846793005ULL
#define RAND_C 1442695040888963407ULL
#else
#define RAND_A 214013U
#define RAND_C 2531011U
#endif

static intnat rnd(void)
{
  static uintnat seed = 0;
  seed = seed * RAND_A + RAND_C;
  return (intnat) seed;
}

/* Test harness */

#define NUM_RANDOM_ITERATIONS 1000000

int main(int argc, char **argv)
{
  for (int i = 0; i < NUM_RANDOM_ITERATIONS; i++) do_test(rnd());
  return 0;
}
