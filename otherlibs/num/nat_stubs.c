/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  Distributed only by permission.                   */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

#define CAML_LIGHT
#include "alloc.h"
#include "memory.h"
#include "mlvalues.h"
#include "nat.h"

#include "BigNum.h"
#include "BntoBnn.h"

/* Stub code for the BigNum package. */

value create_nat(value size)
{
  mlsize_t sz = Long_val(size);

  return alloc(sz, Nat_tag);
}

value set_to_zero_nat(value nat, value ofs, value len)
{
  BnSetToZero(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value blit_nat(value nat1, value ofs1, value nat2, value ofs2, value len)
{
  BnAssign(Bignum_val(nat1), Long_val(ofs1),
           Bignum_val(nat2), Long_val(ofs2),
           Long_val(len));
  return Val_unit;
}

value set_digit_nat(value nat, value ofs, value digit)
{
  BnSetDigit(Bignum_val(nat), Long_val(ofs), Long_val(digit));
  return Val_unit;
}

value nth_digit_nat(value nat, value ofs)
{
  return Val_long(BnGetDigit(Bignum_val(nat), Long_val(ofs)));
}

value num_digits_nat(value nat, value ofs, value len)
{
  return Val_long(BnNumDigits(Bignum_val(nat), Long_val(ofs), Long_val(len)));
}

value num_leading_zero_bits_in_digit(value nat, value ofs)
{
  return
    Val_long(BnNumLeadingZeroBitsInDigit(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_int(value nat, value ofs)
{
  return Val_bool(BnDoesDigitFitInWord(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_zero(value nat, value ofs)
{
  return Val_bool(BnIsDigitZero(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_normalized(value nat, value ofs)
{
  return Val_bool(BnIsDigitNormalized(Bignum_val(nat), Long_val(ofs)));
}

value is_digit_odd(value nat, value ofs)
{
  return Val_bool(BnIsDigitOdd(Bignum_val(nat), Long_val(ofs)));
}

value incr_nat(value nat, value ofs, value len, value carry_in)
{
  return Val_long(BnAddCarry(Bignum_val(nat), Long_val(ofs),
                             Long_val(len), Long_val(carry_in)));
}

value add_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2, value carry_in)
{
  return Val_long(BnAdd(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Long_val(carry_in)));
}

value add_nat(value *argv, int argn)
{
  return add_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

value complement_nat(value nat, value ofs, value len)
{
  BnComplement(Bignum_val(nat), Long_val(ofs), Long_val(len));
  return Val_unit;
}

value decr_nat(value nat, value ofs, value len, value carry_in)
{
  return Val_long(BnSubtractBorrow(Bignum_val(nat), Long_val(ofs),
                                   Long_val(len), Long_val(carry_in)));
}

value sub_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2, value carry_in)
{
  return Val_long(BnSubtract(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Long_val(carry_in)));
}

value sub_nat(value *argv, int argn)
{
  return sub_nat_native(argv[0], argv[1], argv[2], argv[3],
                        argv[4], argv[5], argv[6]);
}

value mult_digit_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2, value nat3, value ofs3)
{
  return
    Val_long(BnMultiplyDigit(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                             Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                             Bignum_val(nat3), Long_val(ofs3)));
}

value mult_digit_nat(value *argv, int argn)
{
  return mult_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                               argv[4], argv[5], argv[6], argv[7]);
}

value mult_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2, value nat3, value ofs3, value len3)
{
  return
    Val_long(BnMultiply(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                        Bignum_val(nat2), Long_val(ofs2), Long_val(len2),
                        Bignum_val(nat3), Long_val(ofs3), Long_val(len3)));
}

value mult_nat(value *argv, int argn)
{
  return mult_nat_native(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value shift_left_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value nbits)
{
  BnShiftLeft(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
              Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value shift_left_nat(value *argv, int argn)
{
  return shift_left_nat_native(argv[0], argv[1], argv[2],
                               argv[3], argv[4], argv[5]);
}

value div_digit_nat_native(value natq, value ofsq, value natr, value ofsr, value nat1, value ofs1, value len1, value nat2, value ofs2)
{
  BnDivideDigit(Bignum_val(natq), Long_val(ofsq),
                Bignum_val(natr), Long_val(ofsr),
                Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value div_digit_nat(value *argv, int argn)
{
  return div_digit_nat_native(argv[0], argv[1], argv[2], argv[3],
                              argv[4], argv[5], argv[6], argv[7], argv[8]);
}

value div_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2)
{
  BnDivide(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
           Bignum_val(nat2), Long_val(ofs2), Long_val(len2));
  return Val_unit;
}

value div_nat(value *argv, int argn)
{
  return div_nat_native(argv[0], argv[1], argv[2],
                        argv[3], argv[4], argv[5]);
}
 
value shift_right_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value nbits)
{
  BnShiftRight(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
              Bignum_val(nat2), Long_val(ofs2), Long_val(nbits));
  return Val_unit;
}

value shift_right_nat(value *argv, int argn)
{
  return shift_right_nat_native(argv[0], argv[1], argv[2],
                               argv[3], argv[4], argv[5]);
}

value compare_digits_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  return Val_long(BnCompareDigits(Bignum_val(nat1), Long_val(ofs1),
                                  Bignum_val(nat2), Long_val(ofs2)));
}

value compare_nat_native(value nat1, value ofs1, value len1, value nat2, value ofs2, value len2)
{
  return Val_long(BnCompare(Bignum_val(nat1), Long_val(ofs1), Long_val(len1),
                            Bignum_val(nat2), Long_val(ofs2), Long_val(len2)));
}

value compare_nat(value *argv, int argn)
{
  return compare_nat_native(argv[0], argv[1], argv[2],
                            argv[3], argv[4], argv[5]);
}

value land_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  BnAndDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lor_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  BnOrDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

value lxor_digit_nat(value nat1, value ofs1, value nat2, value ofs2)
{
  BnXorDigits(Bignum_val(nat1), Long_val(ofs1),
              Bignum_val(nat2), Long_val(ofs2));
  return Val_unit;
}

