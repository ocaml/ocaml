/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* The interface of this file is in "mlvalues.h" and "alloc.h" */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "reverse.h"
#include "stacks.h"

#ifdef ARCH_ALIGN_DOUBLE

CAMLexport double Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

CAMLexport void Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

CAMLexport value copy_double(double d)
{
  value res;

#define Setup_for_gc
#define Restore_after_gc
  Alloc_small(res, Double_wosize, Double_tag);
#undef Setup_for_gc
#undef Restore_after_gc
  Store_double_val(res, d);
  return res;
}

CAMLprim value format_float(value fmt, value arg)
{
#define MAX_DIGITS 350
/* Max number of decimal digits in a "natural" (not artificially padded)
   representation of a float. Can be quite big for %f format.
   Max exponent for IEEE format is 308 decimal digits.
   Rounded up for good measure. */
  char format_buffer[MAX_DIGITS + 20];
  int prec, i;
  char * p;
  char * dest;
  value res;

  prec = MAX_DIGITS;
  for (p = String_val(fmt); *p != 0; p++) {
    if (*p >= '0' && *p <= '9') {
      i = atoi(p) + MAX_DIGITS;
      if (i > prec) prec = i;
      break;
    }
  }
  for( ; *p != 0; p++) {
    if (*p == '.') {
      i = atoi(p+1) + MAX_DIGITS;
      if (i > prec) prec = i;
      break;
    }
  }
  if (prec < sizeof(format_buffer)) {
    dest = format_buffer;
  } else {
    dest = caml_stat_alloc(prec);
  }
  sprintf(dest, String_val(fmt), Double_val(arg));
  res = caml_copy_string(dest);
  if (dest != format_buffer) {
    caml_stat_free(dest);
  }
  return res;
}

CAMLprim value float_of_string(value vs)
{
  char parse_buffer[64];
  char * buf, * src, * dst, * end;
  mlsize_t len;
  double d;

  len = caml_string_length(vs);
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs);
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) caml_failwith("float_of_string");
  d = strtod((const char *) buf, &end);
  if (buf != parse_buffer) caml_stat_free(buf);
  if (end != dst) caml_failwith("float_of_string");
  return copy_double(d);
}

CAMLprim value int_of_float(value f)
{
  return Val_long((long) Double_val(f));
}

CAMLprim value float_of_int(value n)
{
  return copy_double((double) Long_val(n));
}

CAMLprim value neg_float(value f)
{
  return copy_double(- Double_val(f));
}

CAMLprim value abs_float(value f)
{
  return copy_double(fabs(Double_val(f)));
}

CAMLprim value add_float(value f, value g)
{
  return copy_double(Double_val(f) + Double_val(g));
}

CAMLprim value sub_float(value f, value g)
{
  return copy_double(Double_val(f) - Double_val(g));
}

CAMLprim value mul_float(value f, value g)
{
  return copy_double(Double_val(f) * Double_val(g));
}

CAMLprim value div_float(value f, value g)
{
  return copy_double(Double_val(f) / Double_val(g));
}

CAMLprim value exp_float(value f)
{
  return copy_double(exp(Double_val(f)));
}

CAMLprim value floor_float(value f)
{
  return copy_double(floor(Double_val(f)));
}

CAMLprim value fmod_float(value f1, value f2)
{
  return copy_double(fmod(Double_val(f1), Double_val(f2)));
}

CAMLprim value frexp_float(value f)
{
  CAMLparam1 (f);
  CAMLlocal2 (res, mantissa);
  int exponent;

  mantissa = copy_double(frexp (Double_val(f), &exponent));
  res = caml_alloc_tuple(2);
  Field(res, 0) = mantissa;
  Field(res, 1) = Val_int(exponent);
  CAMLreturn (res);
}

CAMLprim value ldexp_float(value f, value i)
{
  return copy_double(ldexp(Double_val(f), Int_val(i)));
}

CAMLprim value log_float(value f)
{
  return copy_double(log(Double_val(f)));
}

CAMLprim value log10_float(value f)
{
  return copy_double(log10(Double_val(f)));
}

CAMLprim value modf_float(value f)
{
#if __SC__
  _float_eval frem;       /* Problem with Apple's <math.h> */
#else
  double frem;
#endif
  CAMLparam1 (f);
  CAMLlocal3 (res, quo, rem);

  quo = copy_double(modf (Double_val(f), &frem));
  rem = copy_double(frem);
  res = caml_alloc_tuple(2);
  Field(res, 0) = quo;
  Field(res, 1) = rem;
  CAMLreturn (res);
}

CAMLprim value sqrt_float(value f)
{
  return copy_double(sqrt(Double_val(f)));
}

CAMLprim value power_float(value f, value g)
{
  return copy_double(pow(Double_val(f), Double_val(g)));
}

CAMLprim value sin_float(value f)
{
  return copy_double(sin(Double_val(f)));
}

CAMLprim value sinh_float(value f)
{
  return copy_double(sinh(Double_val(f)));
}

CAMLprim value cos_float(value f)
{
  return copy_double(cos(Double_val(f)));
}

CAMLprim value cosh_float(value f)
{
  return copy_double(cosh(Double_val(f)));
}

CAMLprim value tan_float(value f)
{
  return copy_double(tan(Double_val(f)));
}

CAMLprim value tanh_float(value f)
{
  return copy_double(tanh(Double_val(f)));
}

CAMLprim value asin_float(value f)
{
  return copy_double(asin(Double_val(f)));
}

CAMLprim value acos_float(value f)
{
  return copy_double(acos(Double_val(f)));
}

CAMLprim value atan_float(value f)
{
  return copy_double(atan(Double_val(f)));
}

CAMLprim value atan2_float(value f, value g)
{
  return copy_double(atan2(Double_val(f), Double_val(g)));
}

CAMLprim value ceil_float(value f)
{
  return copy_double(ceil(Double_val(f)));
}

CAMLprim value eq_float(value f, value g)
{
  return Val_bool(Double_val(f) == Double_val(g));
}

CAMLprim value neq_float(value f, value g)
{
  return Val_bool(Double_val(f) != Double_val(g));
}

CAMLprim value le_float(value f, value g)
{
  return Val_bool(Double_val(f) <= Double_val(g));
}

CAMLprim value lt_float(value f, value g)
{
  return Val_bool(Double_val(f) < Double_val(g));
}

CAMLprim value ge_float(value f, value g)
{
  return Val_bool(Double_val(f) >= Double_val(g));
}

CAMLprim value gt_float(value f, value g)
{
  return Val_bool(Double_val(f) > Double_val(g));
}

CAMLprim value float_compare(value vf, value vg)
{
  double f = Double_val(vf);
  double g = Double_val(vg);
  if (f == g) return Val_int(0);
  if (f < g) return Val_int(-1);
  if (f > g) return Val_int(1);
  /* One or both of f and g is NaN.  Order according to the
     convention NaN = NaN and NaN < x for all other floats x. */
  if (f == f) return Val_int(1);  /* f is not NaN, g is NaN */
  if (g == g) return Val_int(-1); /* g is not NaN, f is NaN */
  return Val_int(0);              /* both f and g are NaN */
}

enum { FP_normal, FP_subnormal, FP_zero, FP_infinite, FP_nan };

CAMLprim value classify_float(value vd)
{
  /* Cygwin 1.3 has problems with fpclassify (PR#1293), so don't use it */
#if defined(fpclassify) && !defined(__CYGWIN32__) && !defined(__MINGW32__)
  switch (fpclassify(Double_val(vd))) {
  case FP_NAN:
    return Val_int(FP_nan);
  case FP_INFINITE:
    return Val_int(FP_infinite);
  case FP_ZERO:
    return Val_int(FP_zero);
  case FP_SUBNORMAL:
    return Val_int(FP_subnormal);
  default: /* case FP_NORMAL */
    return Val_int(FP_normal);
  }
#else
  double d = Double_val(vd);
  uint32 h, l;
#ifdef ARCH_BIG_ENDIAN
  h = ((uint32 *) &d)[0];
  l = ((uint32 *) &d)[1];
#else
  l = ((uint32 *) &d)[0];
  h = ((uint32 *) &d)[1];
#endif
  l = l | (h & 0xFFFFF);
  h = h & 0x7FF00000;
  if ((h | l) == 0)
    return Val_int(FP_zero);
  if (h == 0)
    return Val_int(FP_subnormal);
  if (h == 0x7FF00000) {
    if (l == 0)
      return Val_int(FP_infinite);
    else
      return Val_int(FP_nan);
  }
  return Val_int(FP_normal);
#endif
}

/* The init_ieee_float function should initialize floating-point hardware
   so that it behaves as much as possible like the IEEE standard.
   In particular, return special numbers like Infinity and NaN instead
   of signalling exceptions.  Currently, everyone is in IEEE mode
   at program startup, except FreeBSD prior to 4.0R. */

#ifdef __FreeBSD__
#include <osreldate.h>
#if (__FreeBSD_version < 400017)
#include <floatingpoint.h>
#endif
#endif

void init_ieee_floats(void)
{
#if defined(__FreeBSD__) && (__FreeBSD_version < 400017)
  fpsetmask(0);
#endif
}
