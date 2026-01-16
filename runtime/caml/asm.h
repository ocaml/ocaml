/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*             Bart Jacobs, KU Leuven                                     */
/*             Tom Kelly, OCaml Labs Consultancy, UK                      */
/*                                                                        */
/*   Copyright 2012 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Call Frame Information directives */

#ifdef ASM_CFI_SUPPORTED
#define CFI_ADJUST(n)           .cfi_adjust_cfa_offset n
#define CFI_DEF_CFA_OFFSET(n)   .cfi_def_cfa_offset n
#define CFI_DEF_CFA_REGISTER(r) .cfi_def_cfa_register r
#define CFI_ENDPROC             .cfi_endproc
#define CFI_OFFSET(r, n)        .cfi_offset r, n
#define CFI_REGISTER(r1, r2)    .cfi_register r1, r2
#define CFI_REMEMBER_STATE      .cfi_remember_state
#define CFI_RESTORE(r)          .cfi_restore r
#define CFI_RESTORE_STATE       .cfi_restore_state
#define CFI_SAME_VALUE(r)       .cfi_same_value r
#define CFI_SIGNAL_FRAME        .cfi_signal_frame
#define CFI_STARTPROC           .cfi_startproc
#else
#define CFI_ADJUST(n)
#define CFI_DEF_CFA_OFFSET(n)
#define CFI_DEF_CFA_REGISTER(r)
#define CFI_ENDPROC
#define CFI_OFFSET(r, n)
#define CFI_REGISTER(r1, r2)
#define CFI_REMEMBER_STATE
#define CFI_RESTORE(r)
#define CFI_RESTORE_STATE
#define CFI_SAME_VALUE(r)
#define CFI_SIGNAL_FRAME
#define CFI_STARTPROC
#endif

/* .size and .type directives, with explicit end-of-directive semi-colons */

#ifdef ASM_SIZE_TYPE_DIRECTIVES
#define SIZE_DIRECTIVE(name) \
        .size name, . - name;
#define TYPE_DIRECTIVE(name,ty) \
        .type name, ty;
#else
#define SIZE_DIRECTIVE(name)
#define TYPE_DIRECTIVE(name,ty)
#endif

/* Non-executable stack note */

#ifdef WITH_NONEXECSTACK_NOTE
#define NONEXECSTACK_NOTE       .section .note.GNU-stack,"",%progbits
#else
#define NONEXECSTACK_NOTE
#endif

/******************************************************************************/
/* DWARF */
/******************************************************************************/

/* These constants are taken from:

     DWARF Debugging Information Format, Version 3
     http://dwarfstd.org/doc/Dwarf3.pdf

 */

#define DW_CFA_def_cfa_expression 0x0f
#define DW_OP_breg                0x70
#define DW_OP_deref               0x06
#define DW_OP_plus_uconst         0x23
