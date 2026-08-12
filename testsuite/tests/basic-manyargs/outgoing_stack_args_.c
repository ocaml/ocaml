/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                     Tim McGilchrist, Tarides                           */
/*                                                                        */
/*   Copyright 2026 Tarides                                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/alloc.h>
#include <caml/mlvalues.h>

/* Twelve integer arguments: more than any supported C ABI passes in
   registers (8 on PowerPC ELFv2, arm64 and RISC-V, 6 on amd64 SysV), so the
   surplus is passed in the caller's outgoing-argument area. */

CAMLprim value stack_args_ints(value a, value b, value c, value d,
                               value e, value f, value g, value h,
                               value i, value j, value k, value l)
{
  return Val_long(Long_val(a) + Long_val(b) + Long_val(c) + Long_val(d)
                  + Long_val(e) + Long_val(f) + Long_val(g) + Long_val(h)
                  + Long_val(i) + Long_val(j) + Long_val(k) + Long_val(l));
}

CAMLprim value stack_args_ints_byte(value * argv, int argn)
{
  (void) argn;
  return stack_args_ints(argv[0], argv[1], argv[2], argv[3],
                         argv[4], argv[5], argv[6], argv[7],
                         argv[8], argv[9], argv[10], argv[11]);
}

/* Sixteen floating-point arguments: more than any supported C ABI passes in
   floating-point registers (13 on PowerPC ELFv2, 8 elsewhere).  Integer and
   floating-point arguments are assigned to registers and to stack slots by
   separate rules, so this exercises a different part of the outgoing-argument
   area than [stack_args_ints] above. */

CAMLprim double stack_args_floats(double a, double b, double c, double d,
                                  double e, double f, double g, double h,
                                  double i, double j, double k, double l,
                                  double m, double n, double o, double p)
{
  return a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p;
}

CAMLprim value stack_args_floats_byte(value * argv, int argn)
{
  (void) argn;
  return caml_copy_double(
    stack_args_floats(Double_val(argv[0]), Double_val(argv[1]),
                      Double_val(argv[2]), Double_val(argv[3]),
                      Double_val(argv[4]), Double_val(argv[5]),
                      Double_val(argv[6]), Double_val(argv[7]),
                      Double_val(argv[8]), Double_val(argv[9]),
                      Double_val(argv[10]), Double_val(argv[11]),
                      Double_val(argv[12]), Double_val(argv[13]),
                      Double_val(argv[14]), Double_val(argv[15])));
}
