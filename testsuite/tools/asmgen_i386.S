/**************************************************************************/
/*                                                                        */
/*                                OCaml                                   */
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

/* Linux with ELF binaries does not prefix identifiers with _.
   Linux with a.out binaries, FreeBSD, and NextStep do. */

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define FUNCTION_ALIGN 16
#else
#define G(x) _##x
#define FUNCTION_ALIGN 4
#endif

        .globl  G(call_gen_code)
        .align  FUNCTION_ALIGN
G(call_gen_code):
        pushl %ebp
        movl %esp,%ebp
        pushl %ebx
        pushl %esi
        pushl %edi
        movl 12(%ebp),%eax
        movl 16(%ebp),%ebx
        movl 20(%ebp),%ecx
        movl 24(%ebp),%edx
        call *8(%ebp)
        popl %edi
        popl %esi
        popl %ebx
        popl %ebp
        ret

        .globl  G(caml_c_call)
        .align  FUNCTION_ALIGN
G(caml_c_call):
        jmp     *%eax

        .comm   G(Caml_state), 4

/* Some tests are designed to cause registers to spill; on
 * x86 we require the caml_extra_params symbol from the RTS. */
        .data
        .globl  G(caml_extra_params)
G(caml_extra_params):
#ifndef SYS_solaris
        .space  64
#else
        .zero   64
#endif



#if defined(SYS_linux_elf)
    /* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
#endif
