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

void array_bound_error()
{
  fprintf(stderr, "Fatal error: out-of-bound access in array or string\n");
  exit(2);
}

long r[200];
double d[40];
long x, y;
double f, g;

#define INTTEST(arg,res) \
  { long result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\" for x=%ld and y=%ld: result %ld, expected %ld\n", \
             #arg, #res, x, y, arg, result); \
  }
#define INTFLOATTEST(arg,res) \
  { long result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\" for f=%g and g=%g: result %ld, expected %ld\n", \
             #arg, #res, f, g, arg, result); \
  }
#define FLOATTEST(arg,res) \
  { double result = (res); \
    if (arg < result || arg > result) \
      printf("Failed test \"%s == %s\" for f=%g and g=%g: result %e, expected %e\n", \
             #arg, #res, f, g, arg, result); \
  }
#define FLOATINTTEST(arg,res) \
  { double result = (res); \
    if (arg < result || arg > result) \
      printf("Failed test \"%s == %s\" for x=%ld and y=%ld: result %e, expected %e\n", \
             #arg, #res, x, y, arg, result); \
  }

extern void call_gen_code();
extern void testarith();

void do_test()
{
      call_gen_code(&testarith);

      INTTEST(r[0], 0);
      INTTEST(r[1], 1);
      INTTEST(r[2], -1);
      INTTEST(r[3], 256);
      INTTEST(r[4], 65536);
      INTTEST(r[5], 16777216);
      INTTEST(r[6], -256);
      INTTEST(r[7], -65536);
      INTTEST(r[8], -16777216);

      INTTEST(r[9], (x + y));
      INTTEST(r[10], (x + 1));
      INTTEST(r[11], (x + -1));

      INTTEST(r[12], ((long) ((char *)r + 8)));
      INTTEST(r[13], ((long) ((char *)r + y)));

      INTTEST(r[14], (x - y));
      INTTEST(r[15], (x - 1));
      INTTEST(r[16], (x - -1));

      INTTEST(r[17], ((long) ((char *)r - 8)));
      INTTEST(r[18], ((long) ((char *)r - y)));

      INTTEST(r[19], (x * 2));
      INTTEST(r[20], (2 * x));
      INTTEST(r[21], (x * 16));
      INTTEST(r[22], (16 * x));
      INTTEST(r[23], (x * 12345));
      INTTEST(r[24], (12345 * x));
      INTTEST(r[25], (x * y));

      INTTEST(r[26], (x / 2));
      INTTEST(r[27], (x / 16));
      INTTEST(r[28], (x / 7));
      INTTEST(r[29], (y != 0 ? x / y : 0));

      INTTEST(r[30], (x % 2));
      INTTEST(r[31], (x % 16));
      INTTEST(r[32], (y != 0 ? x % y : 0));

      INTTEST(r[33], (x & y));
      INTTEST(r[34], (x & 3));
      INTTEST(r[35], (3 & x));

      INTTEST(r[36], (x | y));
      INTTEST(r[37], (x | 3));
      INTTEST(r[38], (3 | x));

      INTTEST(r[39], (x ^ y));
      INTTEST(r[40], (x ^ 3));
      INTTEST(r[41], (3 ^ x));

      INTTEST(r[42], (x << y));
      INTTEST(r[43], (x << 1));
      INTTEST(r[44], (x << 8));

      INTTEST(r[45], ((unsigned long) x >> y));
      INTTEST(r[46], ((unsigned long) x >> 1));
      INTTEST(r[47], ((unsigned long) x >> 8));

      INTTEST(r[48], (x >> y));
      INTTEST(r[49], (x >> 1));
      INTTEST(r[50], (x >> 8));

      INTTEST(r[51], (x == y));
      INTTEST(r[52], (x != y));
      INTTEST(r[53], (x < y));
      INTTEST(r[54], (x > y));
      INTTEST(r[55], (x <= y));
      INTTEST(r[56], (x >= y));
      INTTEST(r[57], (x == 1));
      INTTEST(r[58], (x != 1));
      INTTEST(r[59], (x < 1));
      INTTEST(r[60], (x > 1));
      INTTEST(r[61], (x <= 1));
      INTTEST(r[62], (x >= 1));

      INTTEST(r[63], ((char *)x == (char *)y));
      INTTEST(r[64], ((char *)x != (char *)y));
      INTTEST(r[65], ((char *)x < (char *)y));
      INTTEST(r[66], ((char *)x > (char *)y));
      INTTEST(r[67], ((char *)x <= (char *)y));
      INTTEST(r[68], ((char *)x >= (char *)y));
      INTTEST(r[69], ((char *)x == (char *)1));
      INTTEST(r[70], ((char *)x != (char *)1));
      INTTEST(r[71], ((char *)x < (char *)1));
      INTTEST(r[72], ((char *)x > (char *)1));
      INTTEST(r[73], ((char *)x <= (char *)1));
      INTTEST(r[74], ((char *)x >= (char *)1));

      INTTEST(r[75], (x + (y << 1)));
      INTTEST(r[76], (x + (y << 2)));
      INTTEST(r[77], (x + (y << 3)));
      INTTEST(r[78], (x - (y << 1)));
      INTTEST(r[79], (x - (y << 2)));
      INTTEST(r[80], (x - (y << 3)));

      FLOATTEST(d[0], 0.0);
      FLOATTEST(d[1], 1.0);
      FLOATTEST(d[2], -1.0);
      FLOATTEST(d[3], (f + g));
      FLOATTEST(d[4], (f - g));
      FLOATTEST(d[5], (f * g));
      FLOATTEST(d[6], f / g);

      FLOATTEST(d[7], (f + (g + 1.0)));
      FLOATTEST(d[8], (f - (g + 1.0)));
      FLOATTEST(d[9], (f * (g + 1.0)));
      FLOATTEST(d[10], f / (g + 1.0));

      FLOATTEST(d[11], ((f + 1.0) + g));
      FLOATTEST(d[12], ((f + 1.0) - g));
      FLOATTEST(d[13], ((f + 1.0) * g));
      FLOATTEST(d[14], (f + 1.0) / g);

      FLOATTEST(d[15], ((f + 1.0) + (g + 1.0)));
      FLOATTEST(d[16], ((f + 1.0) - (g + 1.0)));
      FLOATTEST(d[17], ((f + 1.0) * (g + 1.0)));
      FLOATTEST(d[18], (f + 1.0) / (g + 1.0));

      INTFLOATTEST(r[81], (f == g));
      INTFLOATTEST(r[82], (f != g));
      INTFLOATTEST(r[83], (f < g));
      INTFLOATTEST(r[84], (f > g));
      INTFLOATTEST(r[85], (f <= g));
      INTFLOATTEST(r[86], (f >= g));

      FLOATINTTEST(d[19], (double) x);
      INTFLOATTEST(r[87], (long) f);

      INTTEST(r[88], (x >= 0) && (x < y));
      INTTEST(r[89], (0 < y));
      INTTEST(r[90], (5 < y));

      INTFLOATTEST(r[91], (f == g));
      INTFLOATTEST(r[92], (f != g));
      INTFLOATTEST(r[93], (f < g));
      INTFLOATTEST(r[94], (f > g));
      INTFLOATTEST(r[95], (f <= g));
      INTFLOATTEST(r[96], (f >= g));

      INTFLOATTEST(r[97], (f + 1.0 == g + 1.0));
      INTFLOATTEST(r[98], (f + 1.0 != g + 1.0));
      INTFLOATTEST(r[99], (f + 1.0 < g + 1.0));
      INTFLOATTEST(r[100], (f + 1.0 > g + 1.0));
      INTFLOATTEST(r[101], (f + 1.0 <= g + 1.0));
      INTFLOATTEST(r[102], (f + 1.0 >= g + 1.0));

      INTFLOATTEST(r[103], (f == g + 1.0));
      INTFLOATTEST(r[104], (f != g + 1.0));
      INTFLOATTEST(r[105], (f < g + 1.0));
      INTFLOATTEST(r[106], (f > g + 1.0));
      INTFLOATTEST(r[107], (f <= g + 1.0));
      INTFLOATTEST(r[108], (f >= g + 1.0));

      INTFLOATTEST(r[109], (f + 1.0 == g));
      INTFLOATTEST(r[110], (f + 1.0 != g));
      INTFLOATTEST(r[111], (f + 1.0 < g));
      INTFLOATTEST(r[112], (f + 1.0 > g));
      INTFLOATTEST(r[113], (f + 1.0 <= g));
      INTFLOATTEST(r[114], (f + 1.0 >= g));

      FLOATINTTEST(d[20], ((double) x) + 1.0);
      INTFLOATTEST(r[115], (long)(f + 1.0));

      FLOATTEST(d[21], f + g);
      FLOATTEST(d[22], g + f);
      FLOATTEST(d[23], f - g);
      FLOATTEST(d[24], g - f);
      FLOATTEST(d[25], f * g);
      FLOATTEST(d[26], g * f);
      FLOATTEST(d[27], f / g);
      FLOATTEST(d[28], g / f);

      FLOATTEST(d[29], (f * 2.0) + g);
      FLOATTEST(d[30], g + (f * 2.0));
      FLOATTEST(d[31], (f * 2.0) - g);
      FLOATTEST(d[32], g - (f * 2.0));
      FLOATTEST(d[33], (f + 2.0) * g);
      FLOATTEST(d[34], g * (f + 2.0));
      FLOATTEST(d[35], (f * 2.0) / g);
      FLOATTEST(d[36], g / (f * 2.0));
}

#ifdef __i386__
#ifdef __linux__
#include <i386/fpu_control.h>
#endif
#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif
#endif

void init_ieee_floats()
{
#ifdef __i386__
#ifdef __linux__
  __setfpucw(_FPU_IEEE);
#endif
#ifdef __FreeBSD__
  fpsetmask(0);
#endif
#endif
}

int main(argc, argv)
     int argc;
     char ** argv;
{
  double weird[4];

  init_ieee_floats();

  if (argc >= 5) {
    x = atoi(argv[1]);
    y = atoi(argv[2]);
    sscanf(argv[3], "%lf", &f);
    sscanf(argv[4], "%lf", &g);
    do_test();
    return 0;
  }
  for(y = -2; y <= 2; y++) {
    for (x = -2; x <= 2; x++) {
      f = x; g = y; do_test();
    }
  }
  weird[0] = 0.0;
  weird[1] = 1.0 / 0.0;         /* +infty */
  weird[2] = -1.0 / 0.0;        /* -infty */
  weird[3] = 0.0 / 0.0;         /* NaN */
  for (x = 0; x < 4; x++) {
    for (y = 0; y < 4; y++) {
      f = weird[x]; g = weird[y]; do_test();
    }
  }
  while(1) {
    x = rand() & 0x1FFFFFFF - 0x10000000;
    y = rand() & 0x1FFFFFFF - 0x10000000;
    f = x / 1e3;
    g = y / 1e3;
    do_test();
    printf("."); fflush(stdout);
  }
  return 0;
}

