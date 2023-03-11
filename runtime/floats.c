/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
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

#define CAML_INTERNALS

/* The interface of this file is in "caml/mlvalues.h" and "caml/alloc.h" */

/* Needed for uselocale */
#define _XOPEN_SOURCE 700

/* Needed for strtod_l */
#define _GNU_SOURCE

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>

#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/misc.h"
#include "caml/reverse.h"
#include "caml/fiber.h"

#if defined(HAS_LOCALE) || defined(__MINGW32__)

#if defined(HAS_LOCALE_H) || defined(__MINGW32__)
#include <locale.h>
#endif

#if defined(HAS_XLOCALE_H)
#include <xlocale.h>
#endif

#if defined(_MSC_VER)
#ifndef locale_t
#define locale_t _locale_t
#endif
#ifndef freelocale
#define freelocale _free_locale
#endif
#ifndef strtod_l
#define strtod_l _strtod_l
#endif
#endif

#endif /* defined(HAS_LOCALE) */

#ifdef _MSC_VER
#include <float.h>
#ifndef isnan
#define isnan _isnan
#endif
#ifndef isfinite
#define isfinite _finite
#endif
#ifndef nextafter
#define nextafter _nextafter
#endif
#endif

#ifndef M_LOG2E
#define M_LOG2E 1.44269504088896340735992468100 /* log_2 (e) */
#endif

#ifdef ARCH_ALIGN_DOUBLE

CAMLexport double caml_Double_val(value val)
{
  union { value v[2]; double d; } buffer;

  CAMLassert(sizeof(double) == 2 * sizeof(value));
  buffer.v[0] = Field(val, 0);
  buffer.v[1] = Field(val, 1);
  return buffer.d;
}

CAMLexport void caml_Store_double_val(value val, double dbl)
{
  union { value v[2]; double d; } buffer;

  CAMLassert(sizeof(double) == 2 * sizeof(value));
  buffer.d = dbl;
  Field(val, 0) = buffer.v[0];
  Field(val, 1) = buffer.v[1];
}

#endif

/*
 OCaml runtime itself doesn't call setlocale, i.e. it is using
 standard "C" locale by default, but it is possible that
 third-party code loaded into process does.
*/
#ifdef HAS_LOCALE
locale_t caml_locale = (locale_t)0;
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
/* there is no analogue to uselocale in MSVC so just set locale for thread */
#define USE_LOCALE setlocale(LC_NUMERIC,"C")
#define RESTORE_LOCALE do {} while(0)
#elif defined(HAS_LOCALE)
#define USE_LOCALE locale_t saved_locale = uselocale(caml_locale)
#define RESTORE_LOCALE uselocale(saved_locale)
#else
#define USE_LOCALE do {} while(0)
#define RESTORE_LOCALE do {} while(0)
#endif

void caml_init_locale(void)
{
#if defined(_MSC_VER) || defined(__MINGW32__)
  _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
#endif
#ifdef HAS_LOCALE
  if ((locale_t)0 == caml_locale)
  {
#if defined(_MSC_VER)
    caml_locale = _create_locale(LC_NUMERIC, "C");
#else
    caml_locale = newlocale(LC_NUMERIC_MASK,"C",(locale_t)0);
#endif
  }
#endif
}

void caml_free_locale(void)
{
#ifdef HAS_LOCALE
  if ((locale_t)0 != caml_locale) freelocale(caml_locale);
  caml_locale = (locale_t)0;
#endif
}

CAMLexport value caml_copy_double(double d)
{
  Caml_check_caml_state();
  value res;

  Alloc_small(res, Double_wosize, Double_tag, Alloc_small_enter_GC);
  Store_double_val(res, d);
  return res;
}

#ifndef FLAT_FLOAT_ARRAY
CAMLexport void caml_Store_double_array_field(value val, mlsize_t i, double dbl)
{
  CAMLparam1 (val);
  value d = caml_copy_double (dbl);

  CAMLassert (Tag_val (val) != Double_array_tag);
  caml_modify (&Field(val, i), d);
  CAMLreturn0;
}
#endif /* ! FLAT_FLOAT_ARRAY */

CAMLprim value caml_format_float(value fmt, value arg)
{
  value res;
  double d = Double_val(arg);

#ifdef HAS_BROKEN_PRINTF
  if (isfinite(d)) {
#endif
    USE_LOCALE;
    res = caml_alloc_sprintf(String_val(fmt), d);
    RESTORE_LOCALE;
#ifdef HAS_BROKEN_PRINTF
  } else {
    if (isnan(d)) {
      res = caml_copy_string("nan");
    } else {
      if (d > 0)
        res = caml_copy_string("inf");
      else
        res = caml_copy_string("-inf");
    }
  }
#endif
  return res;
}

CAMLprim value caml_hexstring_of_float(value arg, value vprec, value vstyle)
{
  union { uint64_t i; double d; } u;
  int sign, exp;
  uint64_t m;
  char buffer[64];
  char * buf, * p;
  intnat prec;
  int d;
  value res;

  /* Allocate output buffer */
  prec = Long_val(vprec);
                  /* 12 chars for sign, 0x, decimal point, exponent */
  buf = (prec + 12 <= 64 ? buffer : caml_stat_alloc(prec + 12));
  /* Extract sign, mantissa, and exponent */
  u.d = Double_val(arg);
  sign = u.i >> 63;
  exp = (u.i >> 52) & 0x7FF;
  m = u.i & (((uint64_t) 1 << 52) - 1);
  /* Put sign */
  p = buf;
  if (sign) {
    *p++ = '-';
  } else {
    switch (Int_val(vstyle)) {
    case '+': *p++ = '+'; break;
    case ' ': *p++ = ' '; break;
    }
  }
  /* Treat special cases */
  if (exp == 0x7FF) {
    char * txt;
    if (m == 0) txt = "infinity"; else txt = "nan";
    memcpy(p, txt, strlen(txt));
    p[strlen(txt)] = 0;
    res = caml_copy_string(buf);
  } else {
    /* Output "0x" prefix */
    *p++ = '0'; *p++ = 'x';
    /* Normalize exponent and mantissa */
    if (exp == 0) {
      if (m != 0) exp = -1022;    /* denormal */
    } else {
      exp = exp - 1023;
      m = m | ((uint64_t) 1 << 52);
    }
    /* If a precision is given, and is small, round mantissa accordingly */
    prec = Long_val(vprec);
    if (prec >= 0 && prec < 13) {
      int i = 52 - prec * 4;
      uint64_t unit = (uint64_t) 1 << i;
      uint64_t half = unit >> 1;
      uint64_t mask = unit - 1;
      uint64_t frac = m & mask;
      m = m & ~mask;
      /* Round to nearest, ties to even */
      if (frac > half || (frac == half && (m & unit) != 0)) {
        m += unit;
      }
    }
    /* Leading digit */
    d = m >> 52;
    *p++ = (d < 10 ? d + '0' : d - 10 + 'a');
    m = (m << 4) & (((uint64_t) 1 << 56) - 1);
    /* Fractional digits.  If a precision is given, print that number of
       digits.  Otherwise, print as many digits as needed to represent
       the mantissa exactly. */
    if (prec >= 0 ? prec > 0 : m != 0) {
      *p++ = '.';
      while (prec >= 0 ? prec > 0 : m != 0) {
        d = m >> 52;
        *p++ = (d < 10 ? d + '0' : d - 10 + 'a');
        m = (m << 4) & (((uint64_t) 1 << 56) - 1);
        prec--;
      }
    }
    *p = 0;
    /* Add exponent */
    res = caml_alloc_sprintf("%sp%+d", buf, exp);
  }
  if (buf != buffer) caml_stat_free(buf);
  return res;
}

static int caml_float_of_hex(const char * s, const char * end, double * res)
{
  int64_t m = 0;                /* the mantissa - top 60 bits at most */
  int n_bits = 0;               /* total number of bits read */
  int m_bits = 0;               /* number of bits in mantissa */
  int x_bits = 0;               /* number of bits after mantissa */
  int dec_point = -1;           /* bit count corresponding to decimal point */
                                /* -1 if no decimal point seen */
  int exp = 0;                  /* exponent */
  char * p;                     /* for converting the exponent */
  double f;

  while (s < end) {
    char c = *s++;
    switch (c) {
    case '.':
      if (dec_point >= 0) return -1; /* multiple decimal points */
      dec_point = n_bits;
      break;
    case 'p': case 'P': {
      long e;
      if (*s == 0) return -1;   /* nothing after exponent mark */
      e = strtol(s, &p, 10);
      if (p != end) return -1;  /* ill-formed exponent */
      /* Handle exponents larger than int by returning 0/infinity directly.
         Mind that INT_MIN/INT_MAX are included in the test so as to capture
         the overflow case of strtol on Win64 -- long and int have the same
         size there. */
      if (e <= INT_MIN) {
        *res = 0.;
        return 0;
      }
      else if (e >= INT_MAX) {
        *res = m == 0 ? 0. : HUGE_VAL;
        return 0;
      }
      /* regular exponent value */
      exp = e;
      s = p;                    /* stop at next loop iteration */
      break;
    }
    default: {                  /* Nonzero digit */
      int d;
      if (c >= '0' && c <= '9') d = c - '0';
      else if (c >= 'A' && c <= 'F') d = c - 'A' + 10;
      else if (c >= 'a' && c <= 'f') d = c - 'a' + 10;
      else return -1;           /* bad digit */
      n_bits += 4;
      if (d == 0 && m == 0) break; /* leading zeros are skipped */
      if (m_bits < 60) {
        /* There is still room in m.  Add this digit to the mantissa. */
        m = (m << 4) + d;
        m_bits += 4;
      } else {
        /* We've already collected 60 significant bits in m.
           Now all we care about is whether there is a nonzero bit
           after. In this case, round m to odd so that the later
           rounding of m to FP produces the correct result. */
        if (d != 0) m |= 1;        /* round to odd */
        x_bits += 4;
      }
      break;
    }
    }
  }
  if (n_bits == 0) return -1;
  /* Convert mantissa to FP.  We use a signed conversion because we can
     (m has 60 bits at most) and because it is faster
     on several architectures. */
  f = (double) (int64_t) m;
  /* Adjust exponent to take decimal point and extra digits into account */
  {
    int adj = x_bits;
    if (dec_point >= 0) adj = adj + (dec_point - n_bits);
    /* saturated addition exp + adj */
    if (adj > 0 && exp > INT_MAX - adj)
      exp = INT_MAX;
    else if (adj < 0 && exp < INT_MIN - adj)
      exp = INT_MIN;
    else
      exp = exp + adj;
  }
  /* Apply exponent if needed */
  if (exp != 0) f = ldexp(f, exp);
  /* Done! */
  *res = f;
  return 0;
}

CAMLprim value caml_float_of_string(value vs)
{
  char parse_buffer[64];
  char * buf, * dst, * end;
  const char *src;
  mlsize_t len;
  int sign;
  double d;

  /* Remove '_' characters before conversion */
  len = caml_string_length(vs);
  buf = len < sizeof(parse_buffer) ? parse_buffer : caml_stat_alloc(len + 1);
  src = String_val(vs);
  dst = buf;
  while (len--) {
    char c = *src++;
    if (c != '_') *dst++ = c;
  }
  *dst = 0;
  if (dst == buf) goto error;
  /* Check for hexadecimal FP constant */
  src = buf;
  sign = 1;
  if (*src == '-') { sign = -1; src++; }
  else if (*src == '+') { src++; };
  if (src[0] == '0' && (src[1] == 'x' || src[1] == 'X')) {
    /* Convert using our hexadecimal FP parser */
    if (caml_float_of_hex(src + 2, dst, &d) == -1) goto error;
    if (sign < 0) d = -d;
  } else {
    /* Convert using strtod */
#if defined(HAS_STRTOD_L) && defined(HAS_LOCALE)
    d = strtod_l((const char *) buf, &end, caml_locale);
#else
    USE_LOCALE;
    d = strtod((const char *) buf, &end);
    RESTORE_LOCALE;
#endif /* HAS_STRTOD_L */
    if (end != dst) goto error;
  }
  if (buf != parse_buffer) caml_stat_free(buf);
  return caml_copy_double(d);
 error:
  if (buf != parse_buffer) caml_stat_free(buf);
  caml_failwith("float_of_string");
  return Val_unit; /* not reached */
}

CAMLprim value caml_int_of_float(value f)
{
  return Val_long((intnat) Double_val(f));
}

CAMLprim value caml_float_of_int(value n)
{
  return caml_copy_double((double) Long_val(n));
}

CAMLprim value caml_neg_float(value f)
{
  return caml_copy_double(- Double_val(f));
}

CAMLprim value caml_abs_float(value f)
{
  return caml_copy_double(fabs(Double_val(f)));
}

CAMLprim value caml_add_float(value f, value g)
{
  return caml_copy_double(Double_val(f) + Double_val(g));
}

CAMLprim value caml_sub_float(value f, value g)
{
  return caml_copy_double(Double_val(f) - Double_val(g));
}

CAMLprim value caml_mul_float(value f, value g)
{
  return caml_copy_double(Double_val(f) * Double_val(g));
}

CAMLprim value caml_div_float(value f, value g)
{
  return caml_copy_double(Double_val(f) / Double_val(g));
}

CAMLprim value caml_exp_float(value f)
{
  return caml_copy_double(exp(Double_val(f)));
}

CAMLexport double caml_exp2(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return exp2(x);
#else
  return pow(2, x);
#endif
}

CAMLprim value caml_exp2_float(value f)
{
  return caml_copy_double(caml_exp2(Double_val(f)));
}

CAMLexport double caml_trunc(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return trunc(x);
#else
  return (x >= 0.0)? floor(x) : ceil(x);
#endif
}

CAMLprim value caml_trunc_float(value f)
{
  return caml_copy_double(caml_trunc(Double_val(f)));
}

CAMLexport double caml_round(double f)
{
#ifdef HAS_WORKING_ROUND
  return round(f);
#else
  union { uint64_t i; double d; } u, pred_one_half; /* predecessor of 0.5 */
  int e;  /* exponent */
  u.d = f;
  e = (u.i >> 52) & 0x7ff; /* - 0x3ff for the actual exponent */
  pred_one_half.i = 0x3FDFFFFFFFFFFFFF; /* 0x1.FFFFFFFFFFFFFp-2 */

  if (isfinite(f) && f != 0.) {
    if (e >= 52 + 0x3ff) return f; /* f is an integer already */
    if (f > 0.0)
      /* If we added 0.5 instead of its predecessor, then the
         predecessor of 0.5 would be rounded to 1. instead of 0. */
      return floor(f + pred_one_half.d);
    else
      return ceil(f - pred_one_half.d);
  }
  else
    return f;
#endif
}

CAMLprim value caml_round_float(value f)
{
  return caml_copy_double(caml_round(Double_val(f)));
}

CAMLprim value caml_floor_float(value f)
{
  return caml_copy_double(floor(Double_val(f)));
}

CAMLexport double caml_nextafter(double x, double y)
{
  return nextafter(x, y);
}

CAMLprim value caml_nextafter_float(value x, value y)
{
  return caml_copy_double(caml_nextafter(Double_val(x), Double_val(y)));
}

#ifndef HAS_WORKING_FMA
union double_as_int64 { double d; uint64_t i; };
#define IEEE754_DOUBLE_BIAS 0x3ff
#define IEEE_EXPONENT(N) (((N) >> 52) & 0x7ff)
#define IEEE_NEGATIVE(N) ((N) >> 63)
//C99 hexa float literals cannot be used, use pow() instead.
#define FL53    (pow(2,53)) //0x1p53
#define FLM53   (pow(2,-53)) //0x1p-53
#define FL54    (pow(2,54)) //0x1p54
#define FLM54   (pow(2,-54)) //0x1p-54
#define FL108   (pow(2,108)) //0x1p108
#define FLM108  (pow(2,-108)) //0x1p-108
#define FLM1074 (pow(2,-1074)) //0x1p-1074
#endif

CAMLexport double caml_fma(double x, double y, double z)
{
#ifdef HAS_WORKING_FMA
  return fma(x, y, z);
#else // Emulation of FMA, from S. Boldo and G. Melquiond, "Emulation
      // of a FMA and Correctly Rounded Sums: Proved Algorithms Using
      // Rounding to Odd," in IEEE Transactions on Computers, vol. 57,
      // no. 4, pp. 462-471, April 2008. Special cases implementation
      // comes from glibc's IEEE754 FMA emulation.
      // Only valid for double precision and round-to-nearest mode.

  union double_as_int64 u, v, w;
  union double_as_int64 ora;
  double mh, ml, xh, xl, yh, yl, t;
  double ah, al;
  double orah, oral;
  double t1, t2;
  double tiny;
  int neg, adjust = 0;
  u.d = x;
  v.d = y;
  w.d = z;

  if ( IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i) >= 0x7FF +
       IEEE754_DOUBLE_BIAS - DBL_MANT_DIG
       || IEEE_EXPONENT(u.i) >= 0x7ff - DBL_MANT_DIG
       || IEEE_EXPONENT(v.i) >= 0x7ff - DBL_MANT_DIG
       || IEEE_EXPONENT(w.i) >= 0x7ff - DBL_MANT_DIG
       || IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i) <=
         IEEE754_DOUBLE_BIAS + DBL_MANT_DIG )
    {
      /* If z is Inf, but x and y are finite, the result should be z
       * rather than NaN. */
      if (IEEE_EXPONENT(w.i) == 0x7ff &&
          IEEE_EXPONENT(u.i) != 0x7ff &&
          IEEE_EXPONENT(v.i) != 0x7ff)
              return (z + x) + y;
      /* If z is zero and x and y are nonzero, compute the result as
         x * y to avoid the wrong sign of a zero result if x * y
         underflows to 0. */
      if (z == 0 && x != 0 && y != 0)
        return x * y;
      /* If x or y or z is Inf/NaN, or if x * y is zero, compute as
         x * y + z.  */
      if (IEEE_EXPONENT(u.i) == 0x7ff
          || IEEE_EXPONENT(v.i) == 0x7ff
          || IEEE_EXPONENT(w.i) == 0x7ff
          || x == 0
          || y == 0)
        return x * y + z;
      /* If fma will certainly overflow, compute as x * y. */
      if ((IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i))
          > 0x7ff + IEEE754_DOUBLE_BIAS)
        return x * y;
      /* If x * y is less than 1/4 of DBL_TRUE_MIN, neither the result
         nor whether there is underflow depends on its exact value,
         only on its sign. */
      if (IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i)
          < IEEE754_DOUBLE_BIAS - DBL_MANT_DIG - 2)
        {
          neg = IEEE_NEGATIVE(u.i) ^ IEEE_NEGATIVE(v.i) ;
          tiny = neg ? -FLM1074 : FLM1074;
          if (IEEE_EXPONENT(w.i) >= 3)
            return tiny + z;
          /* Scaling up, adding TINY and scaling down produces the
             correct result, because in round-to-nearest mode adding
             TINY has no effect and in other modes double rounding is
             harmless. But it may not produce required underflow
             exceptions. */
          v.d = z * FL54 + tiny;
          return v.d * FLM54;
        }
      if (IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i)
          >= 0x7ff + IEEE754_DOUBLE_BIAS - DBL_MANT_DIG)
        {
          /* Compute 1p-53 times smaller result and multiply at the
             end.  */
          if (IEEE_EXPONENT(u.i) > IEEE_EXPONENT(v.i))
            x *= FLM53;
          else
            y *= FLM53;
          /* If x + y exponent is very large and z exponent is very small,
             it doesn't matter if we don't adjust it.  */
          if (IEEE_EXPONENT(w.i) > DBL_MANT_DIG)
            z *= FLM53;
          adjust = 1;
        }
      else if (IEEE_EXPONENT(w.i) >= 0x7ff - DBL_MANT_DIG)
        {
          /* Similarly. If z exponent is very large and x and y
             exponents are very small, adjust them up to avoid
             spurious underflows, rather than down.  */
          if (IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i)
              <= IEEE754_DOUBLE_BIAS + 2 * DBL_MANT_DIG)
            {
              if (IEEE_EXPONENT(u.i) > IEEE_EXPONENT(v.i))
                x *= FL108;
              else
                y *= FL108;
            }
          else if (IEEE_EXPONENT(u.i) > IEEE_EXPONENT(v.i))
            {
              if (IEEE_EXPONENT(u.i) > DBL_MANT_DIG)
                x *= FLM53;
            }
          else if (IEEE_EXPONENT(v.i) > DBL_MANT_DIG)
            y *= FLM53;
          z *= FLM53;
          adjust = 1;
        }
      else if (IEEE_EXPONENT(u.i) >= 0x7ff - DBL_MANT_DIG)
        {
          x *= FLM53;
          y *= FL53;
        }
      else if (IEEE_EXPONENT(v.i) >= 0x7ff - DBL_MANT_DIG)
        {
          y *= FLM53;
          x *= FL53;
        }
      else /* if (IEEE_EXPONENT(u.i) + IEEE_EXPONENT(v.i) <=
              IEEE754_DOUBLE_BIAS + DBL_MANT_DIG) */
        {
          if (IEEE_EXPONENT(u.i) > IEEE_EXPONENT(v.i))
            x *= FL108;
          else
            y *= FL108;
          if (IEEE_EXPONENT(w.i) <= 4 * DBL_MANT_DIG + 6)
            {
              z *= FL108;
              adjust = -1;
            }
        }
    }

  /* Ensure correct sign of exact 0 + 0.  */
  if ((x == 0 || y == 0) && z == 0)
    return x * y + z;

  // Error-free multiplication: mh + ml = x * y
  mh = x * y;
  t = x * 134217729.0;
  xh = t - (t - x);
  xl = x - xh;
  t = y * 134217729.0;
  yh = t - (t - y);
  yl = y - yh;
  ml = xl * yl - (((mh - xh * yh) - xl * yh) - xh * yl);
  // Error-free addition: ah + al = z + mh
  ah = z + mh;
  t = ah - z;
  al = (z - (ah - t)) + (mh - t);

  /* If the result is an exact zero, ensure it has the correct sign. */
  if (ah == 0 && ml == 0)
    return z + mh;

  // Normalize ah, al, ml.
  t1 = al + ml;
  t = t1 - al;
  t2 = (al - (t1 - t)) + (ml - t);
  al = t1;
  ml = t2;
  t1 = ah + al;
  t = t1 - ah;
  t2 = (ah - (t1 - t)) + (al - t);
  ah = t1;
  al = t2;

  // Odd-rounded addition: ora = al + ml.
  orah = al + ml;
  oral = (al - orah) + ml;

  if ( oral != 0.0 )
    {
      ora.d = orah;
      if ( !(ora.i & 1) )
        {
          if ( (oral > 0.0) ^ (orah < 0.0) )
            ora.i++;
          else
            ora.i--;
          orah = ora.d;
        }
    }

  // Rounded addition: ra = ah + orah.
  if ( adjust > 0 )
    return (ah + orah) * FL53;
  else if ( adjust < 0 )
    return (ah + orah) * FLM108;
  else
    return ah + orah;
#endif
}

CAMLprim value caml_fma_float(value f1, value f2, value f3)
{
  return caml_copy_double(caml_fma(Double_val(f1),
                                   Double_val(f2), Double_val(f3)));
}

CAMLprim value caml_fmod_float(value f1, value f2)
{
  return caml_copy_double(fmod(Double_val(f1), Double_val(f2)));
}

CAMLprim value caml_frexp_float(value f)
{
  CAMLparam0 ();
  CAMLlocal1 (mantissa);
  value res;
  int exponent;

  mantissa = caml_copy_double(frexp (Double_val(f), &exponent));
  res = caml_alloc_small(2, 0);
  Field(res, 0) = mantissa;
  Field(res, 1) = Val_int(exponent);
  CAMLreturn (res);
}

// Seems dumb but intnat could not correspond to int type.
double caml_ldexp_float_unboxed(double f, intnat i)
{
  return ldexp(f, (int) i);
}


CAMLprim value caml_ldexp_float(value f, value i)
{
  return caml_copy_double(ldexp(Double_val(f), Int_val(i)));
}

CAMLprim value caml_log_float(value f)
{
  return caml_copy_double(log(Double_val(f)));
}

CAMLprim value caml_log10_float(value f)
{
  return caml_copy_double(log10(Double_val(f)));
}

CAMLexport double caml_log2(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return log2(x);
#else
  return log(x) * M_LOG2E;
#endif
}

CAMLprim value caml_log2_float(value f)
{
  return caml_copy_double(caml_log2(Double_val(f)));
}

CAMLprim value caml_modf_float(value f)
{
  CAMLparam0 ();
  CAMLlocal2 (quo, rem);
  value res;
  double frem;

  quo = caml_copy_double(modf (Double_val(f), &frem));
  rem = caml_copy_double(frem);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = quo;
  Field(res, 1) = rem;
  CAMLreturn (res);
}

CAMLprim value caml_sqrt_float(value f)
{
  return caml_copy_double(sqrt(Double_val(f)));
}

CAMLexport double caml_cbrt(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return cbrt(x);
#else
  static const double third = 1.0 / 3.0;
  double res = exp(third * log(fabs(x)));
  return (x >= 0) ? res : -res;
#endif
}

CAMLprim value caml_cbrt_float(value f)
{
  return caml_copy_double(caml_cbrt(Double_val(f)));
}

CAMLprim value caml_power_float(value f, value g)
{
  return caml_copy_double(pow(Double_val(f), Double_val(g)));
}

CAMLprim value caml_sin_float(value f)
{
  return caml_copy_double(sin(Double_val(f)));
}

CAMLprim value caml_sinh_float(value f)
{
  return caml_copy_double(sinh(Double_val(f)));
}

CAMLprim value caml_cos_float(value f)
{
  return caml_copy_double(cos(Double_val(f)));
}

CAMLprim value caml_cosh_float(value f)
{
  return caml_copy_double(cosh(Double_val(f)));
}

CAMLprim value caml_tan_float(value f)
{
  return caml_copy_double(tan(Double_val(f)));
}

CAMLprim value caml_tanh_float(value f)
{
  return caml_copy_double(tanh(Double_val(f)));
}

CAMLprim value caml_asin_float(value f)
{
  return caml_copy_double(asin(Double_val(f)));
}

CAMLexport double caml_asinh(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return asinh(x);
#else
  return log(x + sqrt(x * x + 1.0));
#endif
}

CAMLprim value caml_asinh_float(value f)
{
  return caml_copy_double(caml_asinh(Double_val(f)));
}

CAMLprim value caml_acos_float(value f)
{
  return caml_copy_double(acos(Double_val(f)));
}

CAMLexport double caml_acosh(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return acosh(x);
#else
  return log(x + sqrt(x * x - 1.0));
#endif
}

CAMLprim value caml_acosh_float(value f)
{
  return caml_copy_double(caml_acosh(Double_val(f)));
}

CAMLprim value caml_atan_float(value f)
{
  return caml_copy_double(atan(Double_val(f)));
}

CAMLexport double caml_atanh(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return atanh(x);
#else
  return 0.5 * log((1.0 + x) / (1.0 - x));
#endif
}

CAMLprim value caml_atanh_float(value f)
{
  return caml_copy_double(caml_atanh(Double_val(f)));
}

CAMLprim value caml_atan2_float(value f, value g)
{
  return caml_copy_double(atan2(Double_val(f), Double_val(g)));
}

CAMLprim value caml_ceil_float(value f)
{
  return caml_copy_double(ceil(Double_val(f)));
}

CAMLexport double caml_hypot(double x, double y)
{
#ifdef HAS_C99_FLOAT_OPS
  return hypot(x, y);
#else
  double tmp, ratio;
  x = fabs(x); y = fabs(y);
  if (x != x) /* x is NaN */
    return y > DBL_MAX ? y : x;  /* PR#6321 */
  if (y != y) /* y is NaN */
    return x > DBL_MAX ? x : y;  /* PR#6321 */
  if (x < y) { tmp = x; x = y; y = tmp; }
  if (x == 0.0) return 0.0;
  ratio = y / x;
  return x * sqrt(1.0 + ratio * ratio);
#endif
}

CAMLprim value caml_hypot_float(value f, value g)
{
  return caml_copy_double(caml_hypot(Double_val(f), Double_val(g)));
}

/* These emulations of expm1() and log1p() are due to William Kahan.
   See http://www.plunk.org/~hatch/rightway.php */
CAMLexport double caml_expm1(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return expm1(x);
#else
  double u = exp(x);
  if (u == 1.)
    return x;
  if (u - 1. == -1.)
    return -1.;
  return (u - 1.) * x / log(u);
#endif
}

CAMLexport double caml_log1p(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return log1p(x);
#else
  double u = 1. + x;
  if (u == 1.)
    return x;
  else
    return log(u) * x / (u - 1.);
#endif
}

CAMLprim value caml_expm1_float(value f)
{
  return caml_copy_double(caml_expm1(Double_val(f)));
}

CAMLprim value caml_log1p_float(value f)
{
  return caml_copy_double(caml_log1p(Double_val(f)));
}

#ifndef HAS_C99_FLOAT_OPS
Caml_inline double simple_erf(double x)
{
  /* This algorithm for calculating the error function is based on formula
     7.1.26 from the "Handbook of Mathematical Functions" by Abramowitz
     and Stegun.  The implementation using Horner's method for evaluating the
     polynomial approximation is derived from Python code by John D. Cook. */
  double a1 =  0.254829592, a2 = -0.284496736, a3 = 1.421413741,
         a4 = -1.453152027, a5 =  1.061405429, p  = 0.3275911,
         t, y;

  int sign = (x >= 0) ? 1 : -1;
  x = fabs(x);
  t = 1.0 / (1.0 + p * x);
  y = 1.0 - (((((a5 *t  + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x);
  return sign * y;
}
#endif

CAMLexport double caml_erf(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return erf(x);
#else
  return simple_erf(x);
#endif
}

CAMLprim value caml_erf_float(value f)
{
  return caml_copy_double(caml_erf(Double_val(f)));
}

CAMLexport double caml_erfc(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return erfc(x);
#else
  return 1.0 - simple_erf(x);
#endif
}

CAMLprim value caml_erfc_float(value f)
{
  return caml_copy_double(caml_erfc(Double_val(f)));
}

union double_as_two_int32 {
    double d;
#if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
    struct { uint32_t h; uint32_t l; } i;
#else
    struct { uint32_t l; uint32_t h; } i;
#endif
};

CAMLexport double caml_copysign(double x, double y)
{
#ifdef HAS_C99_FLOAT_OPS
  return copysign(x, y);
#else
  union double_as_two_int32 ux, uy;
  ux.d = x;
  uy.d = y;
  ux.i.h &= 0x7FFFFFFFU;
  ux.i.h |= (uy.i.h & 0x80000000U);
  return ux.d;
#endif
}

CAMLprim value caml_copysign_float(value f, value g)
{
  return caml_copy_double(caml_copysign(Double_val(f), Double_val(g)));
}

CAMLprim value caml_signbit(double x)
{
#ifdef HAS_C99_FLOAT_OPS
  return Val_bool(signbit(x));
#else
  union double_as_two_int32 ux;
  ux.d = x;
  return Val_bool(ux.i.h >> 31);
#endif
}

CAMLprim value caml_signbit_float(value f)
{
  return caml_signbit(Double_val(f));
}

CAMLprim value caml_neq_float(value f, value g)
{
  return Val_bool(Double_val(f) != Double_val(g));
}

#define DEFINE_NAN_CMP(op) (value f, value g) \
{ \
  return Val_bool(Double_val(f) op Double_val(g)); \
}

intnat caml_float_compare_unboxed(double f, double g)
{
  /* If one or both of f and g is NaN, order according to the convention
     NaN = NaN and NaN < x for all other floats x. */
  /* This branchless implementation is from GPR#164.
     Note that [f == f] if and only if f is not NaN.
     We expand each subresult of the expression to
     avoid sign-extension on 64bit. GPR#2250.
     See also translation of Pcompare_floats in asmcomp/cmmgen.ml  */
  intnat res =
    (intnat)(f > g) - (intnat)(f < g) + (intnat)(f == f) - (intnat)(g == g);
  return res;
}

CAMLprim value caml_eq_float DEFINE_NAN_CMP(==)
CAMLprim value caml_le_float DEFINE_NAN_CMP(<=)
CAMLprim value caml_lt_float DEFINE_NAN_CMP(<)
CAMLprim value caml_ge_float DEFINE_NAN_CMP(>=)
CAMLprim value caml_gt_float DEFINE_NAN_CMP(>)

CAMLprim value caml_float_compare(value vf, value vg)
{
  return Val_int(caml_float_compare_unboxed(Double_val(vf),Double_val(vg)));
}

enum { FP_normal, FP_subnormal, FP_zero, FP_infinite, FP_nan };

value caml_classify_float_unboxed(double vd)
{
#ifdef ARCH_SIXTYFOUR
  union { double d; uint64_t i; } u;
  uint64_t n;
  uint32_t e;

  u.d = vd;
  n = u.i << 1;                 /* shift sign bit off */
  if (n == 0) return Val_int(FP_zero);
  e = n >> 53;                  /* extract exponent */
  if (e == 0) return Val_int(FP_subnormal);
  if (e == 0x7FF) {
    if (n << 11 == 0)           /* shift exponent off */
      return Val_int(FP_infinite);
    else
      return Val_int(FP_nan);
  }
  return Val_int(FP_normal);
#else
  union double_as_two_int32 u;
  uint32_t h, l;

  u.d = vd;
  h = u.i.h;  l = u.i.l;
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

CAMLprim value caml_classify_float(value vd)
{
  return caml_classify_float_unboxed(Double_val(vd));
}
