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

long r[100];
double d[10];
long x, y;
double f, g;

#define INTTEST(arg,res) \
  { long result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\": result %ld, expected %ld\n", \
             #arg, #res, arg, result); \
  }
#define FLOATTEST(arg,res) \
  { double result = (res); \
    if (arg != result) \
      printf("Failed test \"%s == %s\": result %e, expected %e\n", \
             #arg, #res, arg, result); \
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

      INTTEST(r[12], ((int) ((char *)&r + 8)));
      INTTEST(r[13], ((int) ((char *)&r + y)));

      INTTEST(r[14], (x - y));
      INTTEST(r[15], (x - 1));
      INTTEST(r[16], (x - -1));

      INTTEST(r[17], ((int) ((char *)&r - 8)));
      INTTEST(r[18], ((int) ((char *)&r - y)));

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
      FLOATTEST(d[6], (g != 0.0 ? f / g : 0.0));

      INTTEST(r[81], (f == g));
      INTTEST(r[82], (f != g));
      INTTEST(r[83], (f < g));
      INTTEST(r[84], (f > g));
      INTTEST(r[85], (f <= g));
      INTTEST(r[86], (f >= g));

      FLOATTEST(d[7], (double) x);
      INTTEST(r[87], (long) f);

      INTTEST(r[88], (x >= 0) && (x < y));
      INTTEST(r[89], (0 < y));
      INTTEST(r[90], (5 < y));
}



int main(argc, argv)
     int argc;
     char ** argv;
{
  if (argc >= 5) {
    x = atoi(argv[1]);
    y = atoi(argv[2]);
    sscanf(argv[3], "%f", &f);
    sscanf(argv[4], "%f", &g);
    do_test();
    return 0;
  }
  for(y = -2; y <= 2; y++) {
    for (x = -2; x <= 2; x++) {
      f = x; g = y; do_test();
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

