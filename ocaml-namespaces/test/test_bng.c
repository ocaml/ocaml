/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: test_bng.c 5900 2003-11-07 07:59:10Z xleroy $ */

/* Test harness for the BNG primitives.  Use BigNum as a reference. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <BigNum.h>

#include "../../../config/m.h"
#include "bng.h"

#if defined(__GNUC__) && BNG_ASM_LEVEL > 0
#if defined(BNG_ARCH_ia32)
#include "bng_ia32.c"
#elif defined(BNG_ARCH_amd64)
#include "bng_amd64.c"
#elif defined(BNG_ARCH_ppc)
#include "bng_ppc.c"
#elif defined (BNG_ARCH_alpha)
#include "bng_alpha.c"
#elif defined (BNG_ARCH_sparc)
#include "bng_sparc.c"
#elif defined (BNG_ARCH_mips)
#include "bng_mips.c"
#endif
#endif

#include "bng_digit.c"

/* Random generator for digits.  Can either generate "true" PRN numbers
   or numbers consisting of long sequences of 0 and 1 bits. */

static int rand_skewed = 0;
static int rand_runlength = 0;
static int rand_bit = 0;
static bngdigit rand_seed = 0;

static bngdigit randdigit(void)
{
  bngdigit res;
  int i;

  if (rand_skewed) {
    for (i = 0, res = 0; i < BNG_BITS_PER_DIGIT; i++) {
      if (rand_runlength == 0) {
        rand_runlength = 1 + (rand() % (2 * BNG_BITS_PER_DIGIT));
        rand_bit ^= 1;
      }
      res = (res << 1) | rand_bit;
      rand_runlength--;
    }
    return res;
  } else {
    rand_seed = rand_seed * 69069 + 25173;
    return rand_seed;
  }
}

/* Test the operations on digits.  
   This uses double-width integer arithmetic as reference.
   This is only available on 32-bit platforms that support a 64-bit int type.
*/

#if defined(ARCH_UINT64_TYPE) && !defined(ARCH_SIXTYFOUR)

typedef ARCH_UINT64_TYPE dbldigit;

static int test_digit_ops(int i)
{
  bngdigit a1, a2, a3, r1, r2;
  int ci, co, n;

  a1 = randdigit();
  a2 = randdigit();
  a3 = randdigit();
  ci = randdigit() & 1;

  BngAdd2(r1,co,a1,a2);
  if ((dbldigit) r1 + ((dbldigit) co << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 + (dbldigit) a2) {
    printf("Round %d, BngAdd2(%lx,%x,%lx, %lx)\n", i, r1, co, a1, a2);
    return 1;
  }

  BngAdd2Carry(r1,co,a1,a2,ci);
  if ((dbldigit) r1 + ((dbldigit) co << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 + (dbldigit) a2 + (dbldigit) ci) {
    printf("Round %d, BngAdd2Carry(%lx,%x,%lx, %lx, %x)\n", i, r1, co, a1, a2, ci);
    return 1;
  }

  r2 = 0;
  BngAdd3(r1,r2,a1,a2,a3);
  if ((dbldigit) r1 + ((dbldigit) r2 << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 + (dbldigit) a2 + (dbldigit) a3) {
    printf("Round %d, BngAdd3(%lx,%x,%lx, %lx, %lx)\n", i, r1, co, a1, a2, a3);
    return 1;
  }

  BngSub2(r1,co,a1,a2);
  if ((dbldigit) r1 - ((dbldigit) co << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 - (dbldigit) a2) {
    printf("Round %d, BngSub2(%lx,%x,%lx, %lx)\n", i, r1, co, a1, a2);
    return 1;
  }

  BngSub2Carry(r1,co,a1,a2,ci);
  if ((dbldigit) r1 - ((dbldigit) co << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 - (dbldigit) a2 - (dbldigit) ci) {
    printf("Round %d, BngSub2Carry(%lx,%x,%lx, %lx, %x)\n", i, r1, co, a1, a2, ci);
    return 1;
  }

  r2 = 0;
  BngSub3(r1,r2,a1,a2,a3);
  if ((dbldigit) r1 - ((dbldigit) r2 << BNG_BITS_PER_DIGIT)
      != (dbldigit) a1 - (dbldigit) a2 - (dbldigit) a3) {
    printf("Round %d, BngSub3(%lx,%x,%lx, %lx, %lx)\n", i, r1, co, a1, a2, a3);
    return 1;
  }

  BngMult(r1,r2,a1,a2);
  if ((((dbldigit) r1 << BNG_BITS_PER_DIGIT) | (dbldigit) r2)
      != (dbldigit) a1 * (dbldigit) a2) {
    printf("Round %d, BngMult(%lx,%lx,%lx, %lx)\n", i, r1, r2, a1, a2);
    return 1;
  }

  /* Make sure a3 is normalized */
  a3 |= 1L << (BNG_BITS_PER_DIGIT - 1);
  if (a1 < a3) {
    BngDiv(r1,r2,a1,a2,a3);
    if (r1 != (((dbldigit) a1 << BNG_BITS_PER_DIGIT) | (dbldigit) a2) / a3
        ||
        r2 != (((dbldigit) a1 << BNG_BITS_PER_DIGIT) | (dbldigit) a2) % a3)
      {
        printf("Round %d, BngDiv(%lx,%lx,%lx, %lx, %lx)\n", i, r1, r2, a1, a2, a3);
        return 1;
      }
  }

  n = bng_leading_zero_bits(a1);
  if (a1 == 0) {
    if (n != BNG_BITS_PER_DIGIT) {
      printf("Round %d, bng_leading_zero(bits(%lx) = %d", i, a1, n);
      return 1;
    }
  } else {
    if ((a1 << n) >> n != a1 ||
        ((a1 << n) & (1L << (BNG_BITS_PER_DIGIT - 1))) == 0) {
      printf("Round %d, bng_leading_zero(bits(%lx) = %d", i, a1, n);
      return 1;
    }
  }
  return 0;
}

#endif

/* Test the bng operations.  Use BigNum as a reference. */

#define MAX_DIGITS 32

void randbng(bng a, bngsize n)
{
  int i;
  for (i = 0; i < n; i++) a[i] = randdigit();
}

char * bng2string(bng a, bngsize n)
{
  char * buffer = malloc((BNG_BITS_PER_DIGIT / 4 + 1) * MAX_DIGITS);
  char temp[BNG_BITS_PER_DIGIT / 4 + 1];
  int i;

  buffer[0] = 0;
  for (i = n - 1; i >= 0; i--) {
    sprintf(temp, "%lx", a[i]);
    strcat(buffer, temp);
    if (i > 0) strcat(buffer, "_");
  }
  return buffer;
}

int bngsame(bng a, bng b, bngsize n)
{
  int i;
  for (i = 0; i < n; i++)
    if (a[i] != b[i]) return 0;
  return 1;
}

int test_bng_ops(int i)
{
  bngsize p, q;
  bngdigit a[MAX_DIGITS], b[MAX_DIGITS], c[MAX_DIGITS], d[MAX_DIGITS];
  bngdigit f[2 * MAX_DIGITS], g[2 * MAX_DIGITS], h[2 * MAX_DIGITS];
  bngcarry ci, co, cp;
  bngdigit dg, do_, dp;
  int amount;

  /* Determine random lengths p and q between 1 and MAX_DIGITS.
     Ensure p >= q. */
  p = 1 + (rand() % MAX_DIGITS);
  q = 1 + (rand() % MAX_DIGITS);
  if (q > p) { bngsize t = p; p = q; q = t; }

  /* Randomly generate bignums a of size p, b of size q */
  randbng(a, p);
  randbng(b, q);
  ci = rand() & 1;

  /* comparison */
  co = bng_compare(a, p, b, q);
  cp = BnnCompare(a, p, b, q);
  if (co != cp) {
    printf("Round %d, bng_compare(%s, %ld, %s, %ld) = %d\n",
           i, bng2string(a, p), p, bng2string(b, q), q, co);
    return 1;
  }
  co = bng_compare(b, q, a, p);
  cp = BnnCompare(b, q, a, p);
  if (co != cp) {
    printf("Round %d, bng_compare(%s, %ld, %s, %ld) = %d\n",
           i, bng2string(b, q), q, bng2string(a, p), p, co);
    return 1;
  }
  /* add carry */
  bng_assign(c, a, p);
  co = bng_add_carry(c, p, ci);
  BnnAssign(d, a, p);
  cp = BnnAddCarry(d, p, ci);
  if (co != cp || !bngsame(c, d, p)) {
    printf("Round %d, bng_add_carry(%s, %ld, %d) -> %s, %d\n",
           i, bng2string(a, p), p, ci, bng2string(c, p), co);
    return 1;
  }
  /* add */
  bng_assign(c, a, p);
  co = bng_add(c, p, b, q, ci);
  BnnAssign(d, a, p);
  cp = BnnAdd(d, p, b, q, ci);
  if (co != cp || !bngsame(c, d, p)) {
    printf("Round %d, bng_add(%s, %ld, %s, %ld, %d) -> %s, %d\n",
           i, bng2string(a, p), p, bng2string(b, q), q, ci,
           bng2string(c, p), co);
    return 1;
  }
  /* sub carry */
  bng_assign(c, a, p);
  co = bng_sub_carry(c, p, ci);
  BnnAssign(d, a, p);
  cp = BnnSubtractBorrow(d, p, ci ^ 1) ^ 1;
  if (co != cp || !bngsame(c, d, p)) {
    printf("Round %d, bng_sub_carry(%s, %ld, %d) -> %s, %d\n",
           i, bng2string(a, p), p, ci, bng2string(c, p), co);
    return 1;
  }
  /* sub */
  bng_assign(c, a, p);
  co = bng_sub(c, p, b, q, ci);
  BnnAssign(d, a, p);
  cp = BnnSubtract(d, p, b, q, ci ^ 1) ^ 1;
  if (co != cp || !bngsame(c, d, p)) {
    printf("Round %d, bng_sub(%s, %ld, %s, %ld, %d) -> %s, %d\n",
           i, bng2string(a, p), p, bng2string(b, q), q, ci,
           bng2string(c, p), co);
    return 1;
  }
  /* shift left */
  amount = rand() % BNG_BITS_PER_DIGIT;
  bng_assign(c, a, p);
  do_ = bng_shift_left(c, p, amount);
  BnnAssign(d, a, p);
  dp = BnnShiftLeft(d, p, amount);
  if (do_ != dp || !bngsame(c, d, p)) {
    printf("Round %d, bng_shift_left(%s, %ld, %d) -> %s, %ld\n",
           i, bng2string(a, p), p, amount, bng2string(c, p), do_);
    return 1;
  }
  /* shift right */
  amount = rand() % BNG_BITS_PER_DIGIT;
  bng_assign(c, a, p);
  do_ = bng_shift_right(c, p, amount);
  BnnAssign(d, a, p);
  dp = BnnShiftRight(d, p, amount);
  if (do_ != dp || !bngsame(c, d, p)) {
    printf("Round %d, bng_shift_right(%s, %ld, %d) -> %s, %ld\n",
           i, bng2string(a, p), p, amount, bng2string(c, p), do_);
    return 1;
  }
  /* mult_add_digit */
  dg = randdigit();
  if (p >= q + 1) {
    bng_assign(c, a, p);
    co = bng_mult_add_digit(c, p, b, q, dg);
    BnnAssign(d, a, p);
    cp = BnnMultiplyDigit(d, p, b, q, dg);
    if (co != cp || !bngsame(c, d, p)) {
      printf("Round %d, bng_mult_add_digit(%s, %ld, %s, %ld, %ld) -> %s, %d\n",
             i, bng2string(a, p), p, bng2string(b, q), q, dg,
             bng2string(c, p), co);
      return 1;
    }
  }
  /* mult_sub_digit */
  dg = randdigit();
  bng_assign(c, a, p);
  do_ = bng_mult_add_digit(c, p, b, q, dg);
  bng_assign(d, c, p);
  dp = bng_mult_sub_digit(d, p, b, q, dg);
  if (do_ != dp || !bngsame(a, d, p)) {
    printf("Round %d, bng_mult_sub_digit(%s, %ld, %s, %ld, %ld) -> %s, %ld\n",
           i, bng2string(c, p), p, bng2string(b, q), q, dg,
           bng2string(d, p), dp);
    return 1;
  }
  /* mult_add */
  randbng(f, 2*p);
  bng_assign(g, f, 2*p);
  co = bng_mult_add(g, 2*p, a, p, b, q);
  BnnAssign(h, f, 2*p);
  cp = BnnMultiply(h, 2*p, a, p, b, q);
  if (co != cp || !bngsame(g, h, 2*p)) {
    printf("Round %d, bng_mult_add(%s, %ld, %s, %ld, %s, %ld) -> %s, %d\n",
           i, bng2string(f, 2*p), 2*p,
           bng2string(a, p), p,
           bng2string(b, q), q,
           bng2string(g, 2*p), co);
    return 1;
  }
  /* square_add */
  randbng(f, 2*p);
  bng_assign(g, f, 2*p);
  co = bng_square_add(g, 2*p, b, q);
  BnnAssign(h, f, 2*p);
  cp = BnnAdd(h, 2*p, h, 2*p);
  cp += BnnMultiply(h, 2*p, b, q, b, q);
  if (co != cp || !bngsame(g, h, 2*p)) {
    printf("Round %d, bng_square_add(%s, %ld, %s, %ld) -> %s, %d\n",
           i, bng2string(f, 2*p), 2*p,
           bng2string(b, q), q,
           bng2string(g, 2*p), co);
    return 1;
  }
  /* div_rem_digit */
  if (a[p - 1] < dg) {
    do_ = bng_div_rem_digit(c, a, p, dg);
    dp = BnnDivideDigit(d, a, p, dg);
    if (do_ != dp || !bngsame(c, d, p-1)) {
      printf("Round %d, bng_div_rem_digit(%s, %s, %ld, %lx) -> %lx\n",
             i, bng2string(d, p-1), bng2string(a, p), p, dg, do_);
      return 1;
    }
  }
  /* div_rem */
  if (p > q && a[p - 1] < b[q - 1]) {
    bng_assign(c, a, p);
    bng_div_rem(c, p, b, q);
    BnnAssign(d, a, p);
    BnnDivide(d, p, b, q);
    if (!bngsame(c, d, p)) {
      printf("Round %d, bng_div_rem(%s, %ld, %s, %ld) -> %s, %s\n",
             i, bng2string(a, p), p, bng2string(b, q), q,
             bng2string(c + q, p - q),
             bng2string(c, q));
      return 1;
    }
  }
  return 0;
}

int main(int argc, char ** argv)
{
  int niter = 100000;
  int i, err;

  bng_init();
  if (argc >= 2) niter = atoi(argv[1]);
#if defined(ARCH_UINT64_TYPE) && !defined(ARCH_SIXTYFOUR)
  printf("Testing single-digit operations\n");
  for (err = 0, i = 1; i < niter; i++) err += test_digit_ops(i);
  printf("%d rounds performed, %d errors found\n", niter, err);
#endif
  printf("Testing bignum operations\n");
  for (err = 0, i = 1; i < niter; i++) err += test_bng_ops(i);
  printf("%d rounds performed, %d errors found\n", niter, err);
  printf("Testing bignum operations with skewed PRNG\n");
  rand_skewed = 1;
  for (err = 0, i = 1; i < niter; i++) err += test_bng_ops(i);
  printf("%d rounds performed, %d errors found\n", niter, err);
  return 0;
}
