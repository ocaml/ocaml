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

#define CAML_LIGHT
#include "BigNum.h"
#include "BntoBnn.h"
#include "mlvalues.h"
#include "alloc.h"
#include "memory.h"
#include "nat.h"

/* Stub code for the BigNum package. */

value create_nat(size)	/* ML */
     value size;
{
  mlsize_t sz = Long_val(size);

  if (sz < Max_young_wosize) {
    return alloc(sz, Nat_tag);
  } else {
    return alloc_shr(sz, Nat_tag);
  }
}

value set_to_zero_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  BnSetToZero(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value blit_nat(nat1, ofs1, nat2, ofs2, len)	/* ML */
     value nat1, ofs1, nat2, ofs2, len;
{
  BnAssign(Bignum_val(nat1), Long_val(ofs1),
           Bignum_val(nat2), Long_val(ofs2),
           Long_val(len));
  return Val_unit;
}

value set_digit_nat(nat, ofs, digit)	/* ML */
     value nat, ofs, digit;
{
  BnSetDigit(Bignum_val(nat), Long_val(ofs), Long_val(digit));
  return Val_unit;
}

value nth_digit_nat(nat, ofs)	/* ML */
     value nat, ofs;
{
  return Val_long(BnGetDigit(Bignum_val(nat), Long_val(ofs)));
}

value num_digits_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  return Val_long(BnNumDigits(Bignum_val(nat), Long_val(ofs), Long_val(len)));
}

value num_leading_zero_bits_in_digit(nat, ofs)	/* ML */
     value nat, ofs;
{
  return
    Val_long(BnNumLeadingZeroBitsInDigit(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_int(nat, ofs)	/* ML */
     value nat, ofs;
{
  return Val_bool(BnDoesDigitFitInWord(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_zero(nat, ofs)	/* ML */
     value nat, ofs;
{
  return Val_bool(BnIsDigitZero(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_normalized(nat, ofs)	/* ML */
     value nat, ofs;
{
  return Val_bool(BnIsDigitNormalized(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_odd(nat, ofs)	/* ML */
     value nat, ofs;
{
  return Val_bool(BnIsDigitOdd(Bignum_val(nat), Long_val(ofs)));
}

value incr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  return Val_long(BnAddCarry(Bignum_val(nat), Long_val(ofs),
                             Long_val(len), Long_val(carry_in)));
}

value add_nat_native(nat1, ofs1, len1, nat2, ofs2, len2, carry_in)
     value nat1, ofs1, len1, nat2, ofs2, len2, carry_in;
{
  return Val_long(BnAdd(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Long_val(carry_in)));
}

value add_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return add_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

value complement_nat(nat, ofs, len)	/* ML */
     value nat, ofs, len;
{
  BnComplement(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value decr_nat(nat, ofs, len, carry_in)	/* ML */
     value nat, ofs, len, carry_in;
{
  return Val_long(BnSubtractBorrow(Bignum_val(nat), Long_val(ofs),
                                   Long_val(len), Long_val(carry_in)));
}

value sub_nat_native(nat1, ofs1, len1, nat2, ofs2, len2, carry_in)
     value nat1, ofs1, len1, nat2, ofs2, len2, carry_in;
{
  return Val_long(BnSubtract(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Long_val(carry_in)));
}

value sub_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return sub_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

value mult_digit_nat_native(nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3)
     value nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3;
{
  return
    Val_long(BnMultiplyDigit(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Bignum_val(nat3), Long_val(ofs3)));
}

value mult_digit_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return mult_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                               argv[4], argv[5], argv[6], argv[7]);
}

value mult_nat_native(nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3, len3)
     value nat1, ofs1, len1, nat2, ofs2, len2, nat3, ofs3, len3;
{
  return
    Val_long(BnMultiply(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Bignum_val(nat3), Long_val(ofs3), Long_val(len3)));
}

value mult_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return mult_nat_native(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value shift_left_nat_native(nat1, ofs1, len1, nat2, ofs2, nbits)
     value nat1, ofs1, len1, nat2, ofs2, nbits;
{
  BnShiftLeft(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
              Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value shift_left_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return shift_left_nat_native(argv[0], argv[1], argv[2],
                               argv[3], argv[4], argv[5]);
}

value div_digit_nat_native(natq, ofsq, natr, ofsr, nat1, ofs1, len1, nat2, ofs2)
     value natq, ofsq, natr, ofsr, nat1, ofs1, len1, nat2, ofs2;
{
  BnDivideDigit(Bignum_val(natq), Long_val(ofsq),
                Bignum_val(natr), Long_val(ofsr),
                Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value div_digit_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return div_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                              argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value div_nat_native(nat1, ofs1, len1, nat2, ofs2, len2)
     value nat1, ofs1, len1, nat2, ofs2, len2;
{
  BnDivide(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
           Bignum_val(nat2), Long_val(ofs2), Long_val(len2));
  return Val_unit;
}

value div_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return div_nat_native(argv[0], argv[1], argv[2],
                        argv[3], argv[4], argv[5]);
}
 
value shift_right_nat_native(nat1, ofs1, len1, nat2, ofs2, nbits)
     value nat1, ofs1, len1, nat2, ofs2, nbits;
{
  BnShiftRight(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
              Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value shift_right_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return shift_right_nat_native(argv[0], argv[1], argv[2],
                               argv[3], argv[4], argv[5]);
}

value compare_digits_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  return Val_long(BnCompareDigits(Bignum_val(nat1), Long_val(ofs1),
                                  Bignum_val(nat2), Long_val(ofs2)));
}

value compare_nat_native(nat1, ofs1, len1, nat2, ofs2, len2)
     value nat1, ofs1, len1, nat2, ofs2, len2;
{
  return Val_long(BnCompare(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                            Bignum_val(nat2), Long_val(ofs2), Long_val(len2)));
}

value compare_nat(argv, argn)	/* ML */
     value * argv;
     int argn;
{
  return compare_nat_native(argv[0], argv[1], argv[2],
                            argv[3], argv[4], argv[5]);
}

value land_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  BnAndDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lor_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  BnOrDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lxor_digit_nat(nat1, ofs1, nat2, ofs2)	/* ML */
     value nat1, ofs1, nat2, ofs2;
{
  BnXorDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

