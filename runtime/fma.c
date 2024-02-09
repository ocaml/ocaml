/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                         David Allsopp, Tarides                         */
/*                                                                        */
/*   Copyright 2023 David Allsopp Ltd.                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#if defined(_WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif

#include <math.h>

#include "caml/misc.h"

/* caml_fma lives in a unit of its own since it's compiled with hardware fma
   support on Windows, as there are no correct software implementations
   available. */

double caml_fma(double x, double y, double z)
{
#ifdef _MSC_VER
  /* For Microsoft Visual Studio, we use cl's native SEH support to return nan
     on pre-Haswell / pre-Piledriver CPUs (zero-cost on x64; relatively low cost
     on x86). */
  __try {
#endif
    return fma(x, y, z);
#ifdef _MSC_VER
  } __except(GetExceptionCode() == EXCEPTION_ILLEGAL_INSTRUCTION
               ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) {
    return NAN;
  }
#endif
}

/* GCC doesn't have compiler support for SEH, so we have to work a little harder
   for the two mingw-w64 ports and the Cygwin64 port. Use VEH instead. */

#if defined(__MINGW32__) || defined(__CYGWIN__)
double caml_no_fma(void)
{
  return NAN;
}

/* _WIN64 is defined on Cygwin when windows.h is #include'd */
#ifdef _WIN64
#define INSTRUCTION_POINTER Rip
/* On x64, caml_fma will be exactly
     vfmadd132sd %xmm1, %xmm2, %xmm0
     ret
   so Rip will be equal to the function pointer on error */
#define IS_CAML_FMA(ctx) (ctx->Rip == (uintnat)&caml_fma)
#else
#define INSTRUCTION_POINTER Eip
/* On x86, there's a bit more work needed - the vfmadd132sd should be 15 bytes
   after the function pointer, so test within 0x20 for safety */
#define IS_CAML_FMA(ctx) (ctx->Eip >= (uintnat)&caml_fma \
                          && ctx->Eip <= (uintnat)&caml_fma + 0x20)
#endif

static LONG CALLBACK illegal_instruction_handler (EXCEPTION_POINTERS* exn_info)
{
  DWORD code = exn_info->ExceptionRecord->ExceptionCode;
  CONTEXT *ctx = exn_info->ContextRecord;

  if (code == EXCEPTION_ILLEGAL_INSTRUCTION && IS_CAML_FMA(ctx)) {
    ctx->INSTRUCTION_POINTER = (uintnat)&caml_no_fma;
#ifndef _WIN64
    /* Restore the x86 stack pointer */
    ctx->Esp += 12;
#endif
    return EXCEPTION_CONTINUE_EXECUTION;
  }

  return EXCEPTION_CONTINUE_SEARCH;
}

static PVOID handler = NULL;

void caml_win32_fma_detection(void)
{
  handler = AddVectoredExceptionHandler(1, illegal_instruction_handler);
  /* Ignore error - if fma support is lacking and this failed, the Windows
     exception handler will take over. */
}

void caml_win32_unregister_fma_detection(void)
{
  if (handler) {
    RemoveVectoredExceptionHandler(handler);
    handler = NULL;
  }
}
#endif
