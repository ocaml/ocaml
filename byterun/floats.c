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

double Double_val(val)
     value val;
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

void Store_double_val(val, dbl)
     value val;
     double dbl;
{
  union { value v[2]; double d; } buffer;

  Assert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

value copy_double(d)
     double d;
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

value format_float(fmt, arg)    /* ML */
     value fmt, arg;
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

value float_of_string(s)        /* ML */
     value s;
{
  return copy_double(atof(String_val(s)));
}

value int_of_float(f)           /* ML */
     value f;
{
  return Val_long((long) Double_val(f));
}

value float_of_int(n)           /* ML */
     value n;
{
  return copy_double((double) Long_val(n));
}

value neg_float(f)              /* ML */
     value f;
{
  return copy_double(- Double_val(f));
}

value abs_float(f)              /* ML */
     value f;
{
  return copy_double(fabs(Double_val(f)));
}

value add_float(f, g)         /* ML */
     value f, g;
{
  return copy_double(Double_val(f) + Double_val(g));
}

value sub_float(f, g)         /* ML */
     value f, g;
{
  return copy_double(Double_val(f) - Double_val(g));
}

value mul_float(f, g)         /* ML */
     value f, g;
{
  return copy_double(Double_val(f) * Double_val(g));
}

value div_float(f, g)         /* ML */
     value f, g;
{
  double dg = Double_val(g);
  return copy_double(Double_val(f) / dg);
}

value exp_float(f)              /* ML */
     value f;
{
  return copy_double(exp(Double_val(f)));
}

value log_float(f)              /* ML */
     value f;
{
  return copy_double(log(Double_val(f)));
}

value sqrt_float(f)             /* ML */
     value f;
{
  return copy_double(sqrt(Double_val(f)));
}

value power_float(f, g)         /* ML */
     value f, g;
{
  return copy_double(pow(Double_val(f), Double_val(g)));
}

value sin_float(f)              /* ML */
     value f;
{
  return copy_double(sin(Double_val(f)));
}

value cos_float(f)              /* ML */
     value f;
{
  return copy_double(cos(Double_val(f)));
}

value tan_float(f)              /* ML */
     value f;
{
  return copy_double(tan(Double_val(f)));
}

value asin_float(f)             /* ML */
     value f;
{
  return copy_double(asin(Double_val(f)));
}

value acos_float(f)             /* ML */
     value f;
{
  return copy_double(acos(Double_val(f)));
}

value atan_float(f)             /* ML */
     value f;
{
  return copy_double(atan(Double_val(f)));
}

value atan2_float(f, g)        /* ML */
     value f, g;
{
  return copy_double(atan2(Double_val(f), Double_val(g)));
}

value eq_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) == Double_val(g));
}

value neq_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) != Double_val(g));
}

value le_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) <= Double_val(g));
}

value lt_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) < Double_val(g));
}

value ge_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) >= Double_val(g));
}

value gt_float(f, g)        /* ML */
     value f, g;
{
  return Val_bool(Double_val(f) > Double_val(g));
}

/* The init_ieee_float function should initialize floating-point hardware
   so that it behaves as much as possible like the IEEE standard.
   In particular, return special numbers like Infinity and NaN instead
   of signalling exceptions. So far, only the Intel 386 under
   Linux and BSD is not in IEEE mode at program startup. */

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
