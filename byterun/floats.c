/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "misc.h"
#include "stacks.h"

#ifdef ARCH_ALIGN_DOUBLE

double Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

void Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

value copy_double(double d)
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

value format_float(value fmt, value arg)    /* ML */
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
  if (prec <= sizeof(format_buffer)) {
    dest = format_buffer;
  } else {
    dest = stat_alloc(prec);
  }
  sprintf(dest, String_val(fmt), Double_val(arg));
  res = copy_string(dest);
  if (dest != format_buffer) {
    stat_free(dest);
  }
  return res;
}

value float_of_string(value s)        /* ML */
{
  return copy_double(atof(String_val(s)));
}

value int_of_float(value f)           /* ML */
{
  return Val_long((long) Double_val(f));
}

value float_of_int(value n)           /* ML */
{
  return copy_double((double) Long_val(n));
}

value neg_float(value f)              /* ML */
{
  return copy_double(- Double_val(f));
}

value abs_float(value f)              /* ML */
{
  return copy_double(fabs(Double_val(f)));
}

value add_float(value f, value g)         /* ML */
{
  return copy_double(Double_val(f) + Double_val(g));
}

value sub_float(value f, value g)         /* ML */
{
  return copy_double(Double_val(f) - Double_val(g));
}

value mul_float(value f, value g)         /* ML */
{
  return copy_double(Double_val(f) * Double_val(g));
}

value div_float(value f, value g)         /* ML */
{
  return copy_double(Double_val(f) / Double_val(g));
}

value exp_float(value f)              /* ML */
{
  return copy_double(exp(Double_val(f)));
}

value floor_float(value f)              /* ML */
{
  return copy_double(floor(Double_val(f)));
}

value fmod_float(value f1, value f2)              /* ML */
{
  return copy_double(fmod(Double_val(f1), Double_val(f2)));
}

value frexp_float(value f)              /* ML */
{
  int exponent;
  value res;
  value mantissa = copy_double(frexp (Double_val(f), &exponent));
  Begin_root(mantissa);
    res = alloc_tuple(2);
    Field(res, 0) = mantissa;
    Field(res, 1) = Val_int(exponent);
  End_roots();
  return res;
}

value ldexp_float(value f, value i)              /* ML */
{
  return copy_double(ldexp(Double_val(f), Int_val(i)));
}

value log_float(value f)              /* ML */
{
  return copy_double(log(Double_val(f)));
}

value log10_float(value f)              /* ML */
{
  return copy_double(log10(Double_val(f)));
}

value modf_float(value f)              /* ML */
{
#if macintosh
  _float_eval frem;
#else
  double frem;
#endif
  value res;
  value quo = Val_unit, rem = Val_unit;

  Begin_roots2(quo,rem);
    quo = copy_double(modf (Double_val(f), &frem));
    rem = copy_double(frem);
    res = alloc_tuple(2);
    Field(res, 0) = quo;
    Field(res, 1) = rem;
  End_roots();
  return res;
}

value sqrt_float(value f)             /* ML */
{
  return copy_double(sqrt(Double_val(f)));
}

value power_float(value f, value g)         /* ML */
{
  return copy_double(pow(Double_val(f), Double_val(g)));
}

value sin_float(value f)              /* ML */
{
  return copy_double(sin(Double_val(f)));
}

value sinh_float(value f)              /* ML */
{
  return copy_double(sinh(Double_val(f)));
}

value cos_float(value f)              /* ML */
{
  return copy_double(cos(Double_val(f)));
}

value cosh_float(value f)              /* ML */
{
  return copy_double(cosh(Double_val(f)));
}

value tan_float(value f)              /* ML */
{
  return copy_double(tan(Double_val(f)));
}

value tanh_float(value f)              /* ML */
{
  return copy_double(tanh(Double_val(f)));
}

value asin_float(value f)             /* ML */
{
  return copy_double(asin(Double_val(f)));
}

value acos_float(value f)             /* ML */
{
  return copy_double(acos(Double_val(f)));
}

value atan_float(value f)             /* ML */
{
  return copy_double(atan(Double_val(f)));
}

value atan2_float(value f, value g)        /* ML */
{
  return copy_double(atan2(Double_val(f), Double_val(g)));
}

value ceil_float(value f)              /* ML */
{
  return copy_double(ceil(Double_val(f)));
}

value eq_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) == Double_val(g));
}

value neq_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) != Double_val(g));
}

value le_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) <= Double_val(g));
}

value lt_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) < Double_val(g));
}

value ge_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) >= Double_val(g));
}

value gt_float(value f, value g)        /* ML */
{
  return Val_bool(Double_val(f) > Double_val(g));
}

/* The init_ieee_float function should initialize floating-point hardware
   so that it behaves as much as possible like the IEEE standard.
   In particular, return special numbers like Infinity and NaN instead
   of signalling exceptions. So far, only the Intel 386 under
   FreeBSD is not in IEEE mode at program startup.  */

#ifdef __i386__
#ifdef __FreeBSD__
#include <floatingpoint.h>
#endif
#endif

void init_ieee_floats(void)
{
#ifdef __i386__
#ifdef __FreeBSD__
  fpsetmask(0);
#endif
#endif
}
