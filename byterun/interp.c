/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* The bytecode interpreter */
#include <stdio.h>
#include "alloc.h"
#include "backtrace.h"
#include "callback.h"
#include "debugger.h"
#include "fail.h"
#include "fix_code.h"
#include "instrtrace.h"
#include "instruct.h"
#include "interp.h"
#include "major_gc.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "fiber.h"
#include "domain.h"
#include "globroots.h"
#include "startup.h"

/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
        caml_trapsp pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable caml_extern_sp. */

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name
#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#    define Jumptbl_base ((char *) &&lbl_ACC0)
#  else
#    define Jumptbl_base ((char *) 0)
#    define jumptbl_base ((char *) 0)
#  endif
#  ifdef DEBUG
#    define Next goto next_instr
#  else
#    define Next goto *(void *)(jumptbl_base + *pc++)
#  endif
#else
#  define Instruct(name) case name
#  define Next break
#endif

/* GC interface */

#define Setup_for_gc \
  { sp -= 2; sp[0] = accu; sp[1] = env; caml_extern_sp = sp; }
#define Restore_after_gc \
  { sp = caml_extern_sp; accu = sp[0]; env = sp[1]; sp += 2; }
#define Setup_for_c_call \
  { saved_pc = pc; *--sp = env; caml_extern_sp = sp; }
#define Restore_after_c_call \
  { sp = caml_extern_sp; env = *sp++; saved_pc = NULL; }

/* Context switch interface */
#define Setup_for_context_switch \
  { sp -= 3; sp[0] = Val_pc(pc); sp[1] = env; \
    sp[2] = Val_long(extra_args); caml_extern_sp = sp; }
#define Setup_for_tail_context_switch \
  { caml_extern_sp = sp + *pc; }
#define Restore_after_context_switch \
  { sp = caml_extern_sp; }

/* An event frame must look like accu + a C_CALL frame + a RETURN 1 frame */
#define Setup_for_event \
  { sp -= 6; \
    sp[0] = accu; /* accu */ \
    sp[1] = Val_unit; /* C_CALL frame: dummy environment */ \
    sp[2] = Val_unit; /* RETURN frame: dummy local 0 */ \
    sp[3] = Val_pc(pc); /* RETURN frame: saved return address */  \
    sp[4] = env; /* RETURN frame: saved environment */ \
    sp[5] = Val_long(extra_args); /* RETURN frame: saved extra args */ \
    caml_extern_sp = sp; }
#define Restore_after_event \
  { sp = caml_extern_sp; accu = sp[0]; \
    pc = Pc_val(sp[3]); env = sp[4]; extra_args = Long_val(sp[5]); \
    sp += 6; }

/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     caml_extern_sp = sp; }
#define Restore_after_debugger { sp += 4; }

#ifdef THREADED_CODE
#define Restart_curr_instr \
  goto *(jumptable[caml_saved_code[pc - 1 - caml_start_code]])
#else
#define Restart_curr_instr \
  curr_instr = caml_saved_code[pc - 1 - caml_start_code]; \
  goto dispatch_instr
#endif

/* Register optimization.
   Some compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#if defined(__GNUC__) && !defined(DEBUG) && !defined(__INTEL_COMPILER) \
    && !defined(__llvm__)
#ifdef __mips__
#define PC_REG asm("$16")
#define SP_REG asm("$17")
#define ACCU_REG asm("$18")
#endif
#ifdef __sparc__
#define PC_REG asm("%l0")
#define SP_REG asm("%l1")
#define ACCU_REG asm("%l2")
#endif
#ifdef __alpha__
#ifdef __CRAY__
#define PC_REG asm("r9")
#define SP_REG asm("r10")
#define ACCU_REG asm("r11")
#define JUMPTBL_BASE_REG asm("r12")
#else
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define ACCU_REG asm("$11")
#define JUMPTBL_BASE_REG asm("$12")
#endif
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG
#endif
#if defined(__ppc__) || defined(__ppc64__)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define ACCU_REG asm("28")
#endif
#ifdef __hppa__
#define PC_REG asm("%r18")
#define SP_REG asm("%r17")
#define ACCU_REG asm("%r16")
#endif
#ifdef __mc68000__
#define PC_REG asm("a5")
#define SP_REG asm("a4")
#define ACCU_REG asm("d7")
#endif
/* PR#4953: these specific registers not available in Thumb mode */
#if defined (__arm__) && !defined(__thumb__)
#define PC_REG asm("r6")
#define SP_REG asm("r8")
#define ACCU_REG asm("r7")
#endif
#ifdef __ia64__
#define PC_REG asm("36")
#define SP_REG asm("37")
#define ACCU_REG asm("38")
#define JUMPTBL_BASE_REG asm("39")
#endif
#ifdef __x86_64__
#define PC_REG asm("%r15")
#define SP_REG asm("%r14")
#define ACCU_REG asm("%r13")
#endif
#ifdef __aarch64__
#define PC_REG asm("%x19")
#define SP_REG asm("%x20")
#define ACCU_REG asm("%x21")
#define JUMPTBL_BASE_REG asm("%x22")
#endif
#endif

#ifdef DEBUG
static __thread intnat caml_bcodcount;
#endif

/* The interpreter itself */
value caml_interprete(code_t prog, asize_t prog_size)
{
#ifdef PC_REG
  register code_t pc PC_REG;
  register value * sp SP_REG;
  register value accu ACCU_REG;
#else
  register code_t pc;
  register value * sp;
  register value accu;
#endif
#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#ifdef JUMPTBL_BASE_REG
  register char * jumptbl_base JUMPTBL_BASE_REG;
#else
  register char * jumptbl_base;
#endif
#endif
  value env;
  intnat extra_args;
  struct caml_exception_context * initial_external_raise;
  int initial_stack_words;
  value initial_parent_stack;
  intnat initial_trap_sp_off;
  volatile code_t saved_pc = NULL;
  struct longjmp_buffer raise_buf;
  struct caml_exception_context exception_ctx = { &raise_buf, caml_local_roots };
#ifndef THREADED_CODE
  opcode_t curr_instr;
#endif

#ifdef THREADED_CODE
  static void * jumptable[] = {
#    include "jumptbl.h"
  };
#endif

  if (prog == NULL) {           /* Interpreter is initializing */
#ifdef THREADED_CODE
    caml_instr_table = (char **) jumptable;
    caml_instr_base = Jumptbl_base;
#endif
    caml_global_data = caml_create_root(Val_unit);
    caml_init_callbacks();
    caml_init_fibers();
    return Val_unit;
  }

#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  jumptbl_base = Jumptbl_base;
#endif
  initial_trap_sp_off = caml_trap_sp_off;
  initial_parent_stack = caml_parent_stack;
  initial_stack_words = caml_stack_high - caml_extern_sp;
  initial_external_raise = caml_external_raise;
  caml_callback_depth++;
  saved_pc = NULL;

  if (sigsetjmp(raise_buf.buf, 0)) {
    /* no local variables read here */
    sp = caml_extern_sp;
    accu = caml_exn_bucket;
    pc = saved_pc; saved_pc = NULL;
    if (pc != NULL) pc += 2;
        /* +2 adjustement for the sole purpose of backtraces */
    goto raise_exception;
  }
  caml_external_raise = &exception_ctx;

  caml_trap_sp_off = 1;
  caml_parent_stack = Val_long(0);

  sp = caml_extern_sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);

#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (caml_icount-- == 0) caml_stop_here ();
  Assert(sp == caml_stack_high || caml_on_current_stack(sp));
#endif
  goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
#else
  while(1) {
#ifdef DEBUG

    Assert(!Is_foreign(accu));
    Assert(!Is_foreign(env));
#if 0
    {
      value* s;
      for (s = sp; s < caml_stack_high; s++) {
        Assert(*s != Debug_free_minor);
        Assert(!Is_foreign(*s));
        if (Is_minor(*s)) Assert(caml_young_ptr < (char*)*s);
      }
    }
#endif
    caml_bcodcount++;
    if (caml_icount-- == 0) caml_stop_here ();
    if (caml_startup_params.trace_flag>1) printf("\n##%ld\n", caml_bcodcount);
    if (caml_startup_params.trace_flag) caml_disasm_instr(pc);
    if (caml_startup_params.trace_flag>1) {
      printf("env=");
      caml_trace_value_file(env,prog,prog_size,stdout);
      putchar('\n');
      caml_trace_accu_sp_file(accu,sp,prog,prog_size,stdout);
      fflush(stdout);
    };
    Assert(sp == caml_stack_high || caml_on_current_stack(sp));
#endif
    curr_instr = *pc++;

  dispatch_instr:
    switch(curr_instr) {
#endif

/* Basic stack operations */

    Instruct(ACC0):
      accu = sp[0]; Next;
    Instruct(ACC1):
      accu = sp[1]; Next;
    Instruct(ACC2):
      accu = sp[2]; Next;
    Instruct(ACC3):
      accu = sp[3]; Next;
    Instruct(ACC4):
      accu = sp[4]; Next;
    Instruct(ACC5):
      accu = sp[5]; Next;
    Instruct(ACC6):
      accu = sp[6]; Next;
    Instruct(ACC7):
      accu = sp[7]; Next;

    Instruct(PUSH): Instruct(PUSHACC0):
      *--sp = accu; Next;
    Instruct(PUSHACC1):
      *--sp = accu; accu = sp[1]; Next;
    Instruct(PUSHACC2):
      *--sp = accu; accu = sp[2]; Next;
    Instruct(PUSHACC3):
      *--sp = accu; accu = sp[3]; Next;
    Instruct(PUSHACC4):
      *--sp = accu; accu = sp[4]; Next;
    Instruct(PUSHACC5):
      *--sp = accu; accu = sp[5]; Next;
    Instruct(PUSHACC6):
      *--sp = accu; accu = sp[6]; Next;
    Instruct(PUSHACC7):
      *--sp = accu; accu = sp[7]; Next;

    Instruct(PUSHACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ACC):
      accu = sp[*pc++];
      Next;

    Instruct(POP):
      sp += *pc++;
      Next;
    Instruct(ASSIGN):
      sp[*pc++] = accu;
      accu = Val_unit;
      Next;

/* Access in heap-allocated environment */

    Instruct(ENVACC1):
      accu = FieldImm(env, 1); Next;
    Instruct(ENVACC2):
      accu = FieldImm(env, 2); Next;
    Instruct(ENVACC3):
      accu = FieldImm(env, 3); Next;
    Instruct(ENVACC4):
      accu = FieldImm(env, 4); Next;

    Instruct(PUSHENVACC1):
      *--sp = accu; accu = FieldImm(env, 1); Next;
    Instruct(PUSHENVACC2):
      *--sp = accu; accu = FieldImm(env, 2); Next;
    Instruct(PUSHENVACC3):
      *--sp = accu; accu = FieldImm(env, 3); Next;
    Instruct(PUSHENVACC4):
      *--sp = accu; accu = FieldImm(env, 4); Next;

    Instruct(PUSHENVACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ENVACC):
      accu = FieldImm(env, *pc++);
      Next;

/* Function application */

    Instruct(PUSH_RETADDR): {
      sp -= 3;
      sp[0] = Val_pc(pc + *pc);
      sp[1] = env;
      sp[2] = Val_long(extra_args);
      pc++;
      Next;
    }
    Instruct(APPLY): {
      extra_args = *pc - 1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPLY1): {
      value arg1 = sp[0];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = Val_pc(pc);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      goto check_stacks;
    }
    Instruct(APPLY2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = Val_pc(pc);
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 1;
      goto check_stacks;
    }
    Instruct(APPLY3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp -= 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      sp[3] = Val_pc(pc);
      sp[4] = env;
      sp[5] = Val_long(extra_args);
      pc = Code_val(accu);
      env = accu;
      extra_args = 2;
      goto check_stacks;
    }

    Instruct(APPTERM): {
      int nargs = *pc++;
      int slotsize = *pc;
      value * newsp;
      int i;
      /* Slide the nargs bottom words of the current frame to the top
         of the frame, and discard the remainder of the frame */
      newsp = sp + slotsize - nargs;
      for (i = nargs - 1; i >= 0; i--) newsp[i] = sp[i];
      sp = newsp;
      pc = Code_val(accu);
      env = accu;
      extra_args += nargs - 1;
      goto check_stacks;
    }
    Instruct(APPTERM1): {
      value arg1 = sp[0];
      sp = sp + *pc - 1;
      sp[0] = arg1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPTERM2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp = sp + *pc - 2;
      sp[0] = arg1;
      sp[1] = arg2;
      pc = Code_val(accu);
      env = accu;
      extra_args += 1;
      goto check_stacks;
    }
    Instruct(APPTERM3): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      value arg3 = sp[2];
      sp = sp + *pc - 3;
      sp[0] = arg1;
      sp[1] = arg2;
      sp[2] = arg3;
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

    Instruct(RETURN): {
      sp += *pc++;
      if (extra_args > 0) {
        extra_args--;
        pc = Code_val(accu);
        env = accu;
      } else {
        pc = Pc_val(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }

    Instruct(RESTART): {
      int num_args = Wosize_val(env) - 2;
      int i;
      sp -= num_args;
      for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 2);
      env = Field(env, 1);
      extra_args += num_args;
      Next;
    }

    Instruct(GRAB): {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
      } else {
        mlsize_t num_args, i;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 2, Closure_tag);
        Init_field(accu, 1, env);
        for (i = 0; i < num_args; i++) Init_field(accu, i + 2, sp[i]);
        Init_field(accu, 0, Val_bytecode(pc - 3)); /* Point to the preceding RESTART instr. */
        sp += num_args;
        pc = Pc_val(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;
    }

    Instruct(CLOSURE): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      if (nvars < Max_young_wosize) {
        /* nvars + 1 <= Max_young_wosize, can allocate in minor heap */
        Alloc_small(accu, 1 + nvars, Closure_tag);
        for (i = 0; i < nvars; i++) Init_field(accu, i + 1, sp[i]);
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(1 + nvars, Closure_tag);
        for (i = 0; i < nvars; i++)
          caml_initialize_field(accu, i + 1, sp[i]);
      }
      /* The code pointer is not in the heap, so no need to go through
         caml_initialize. */
      Init_field(accu, 0, Val_bytecode(pc + *pc));
      pc++;
      sp += nvars;
      Next;
    }

    Instruct(CLOSUREREC): {
      int nfuncs = *pc++;
      int nvars = *pc++;
      int i, field;
      int var_offset = nfuncs * 2 - 1;
      int blksize = var_offset + nvars;
      if (nvars > 0) *--sp = accu;
      if (blksize <= Max_young_wosize) {
        Alloc_small(accu, blksize, Closure_tag);
        for (i = 0; i < nvars; i++) {
          Init_field(accu, var_offset + i, sp[i]);
        }
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(blksize, Closure_tag);
        for (i = 0; i < nvars; i++)
          caml_initialize_field(accu, var_offset + i,sp[i]);
      }
      sp += nvars;
      /* The code pointers and infix headers are not in the heap,
         so no need to go through caml_initialize. */
      Init_field(accu, 0, Val_bytecode(pc + pc[0]));
      *--sp = accu;
      field = 1;
      for (i = 1; i < nfuncs; i++) {
        Init_field(accu, field, Make_header(i * 2, Infix_tag, 0)); /* color irrelevant */
        field++;
        Init_field(accu, field, Val_bytecode (pc + pc[i]));
        *--sp = (value) (Op_val(accu) + field);
        field++;
      }
      pc += nfuncs;
      Next;
    }

    Instruct(PUSHOFFSETCLOSURE):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE):
      accu = env + *pc++ * sizeof(value); Next;

    Instruct(PUSHOFFSETCLOSUREM2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSUREM2):
      accu = env - 2 * sizeof(value); Next;
    Instruct(PUSHOFFSETCLOSURE0):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE0):
      accu = env; Next;
    Instruct(PUSHOFFSETCLOSURE2):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE2):
      accu = env + 2 * sizeof(value); Next;


/* Access to global variables */

    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field(caml_read_root(caml_global_data), *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field(caml_read_root(caml_global_data), *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL):
      caml_modify_field(caml_read_root(caml_global_data), *pc, accu);
      accu = Val_unit;
      pc++;
      Next;

/* Allocation of blocks */

    Instruct(PUSHATOM0):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM0):
      accu = Atom(0); Next;

    Instruct(PUSHATOM):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ATOM):
      accu = Atom(*pc++); Next;

    Instruct(MAKEBLOCK): {
      mlsize_t wosize = *pc++;
      tag_t tag = *pc++;
      mlsize_t i;
      value block;
      if (wosize <= Max_young_wosize) {
        Alloc_small(block, wosize, tag);
        Init_field(block, 0, accu);
        for (i = 1; i < wosize; i++) Init_field(block, i, *sp++);
      } else {
        Setup_for_gc;
        block = caml_alloc_shr(wosize, tag);
        Restore_after_gc;
        caml_initialize_field(block, 0, accu);
        for (i = 1; i < wosize; i++) caml_initialize_field(block, i, *sp++);
      }
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag);
      Init_field(block, 0, accu);
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag);
      Init_field(block, 0, accu);
      Init_field(block, 1, sp[0]);
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag);
      Init_field(block, 0, accu);
      Init_field(block, 1, sp[0]);
      Init_field(block, 2, sp[1]);
      sp += 2;
      accu = block;
      Next;
    }
    Instruct(MAKEFLOATBLOCK): {
      mlsize_t size = *pc++;
      mlsize_t i;
      value block;
      if (size <= Max_young_wosize / Double_wosize) {
        Alloc_small(block, size * Double_wosize, Double_array_tag);
      } else {
        Setup_for_gc;
        block = caml_alloc_shr(size * Double_wosize, Double_array_tag);
        Restore_after_gc;
      }
      Store_double_field(block, 0, Double_val(accu));
      for (i = 1; i < size; i++){
        Store_double_field(block, i, Double_val(*sp));
        ++ sp;
      }
      accu = block;
      Next;
    }

/* Access to components of blocks */

    Instruct(GETFIELD0):
      accu = FieldImm(accu, 0); Next;
    Instruct(GETFIELD1):
      accu = FieldImm(accu, 1); Next;
    Instruct(GETFIELD2):
      accu = FieldImm(accu, 2); Next;
    Instruct(GETFIELD3):
      accu = FieldImm(accu, 3); Next;
    Instruct(GETFIELD):
      accu = FieldImm(accu, *pc); pc++; Next;
    Instruct(GETMUTABLEFIELD0):
      accu = Field(accu, 0); Next;
    Instruct(GETMUTABLEFIELD1):
      accu = Field(accu, 1); Next;
    Instruct(GETMUTABLEFIELD2):
      accu = Field(accu, 2); Next;
    Instruct(GETMUTABLEFIELD3):
      accu = Field(accu, 3); Next;
    Instruct(GETMUTABLEFIELD):
      accu = Field(accu, *pc); pc++; Next;
    Instruct(GETFLOATFIELD): {
      double d = Double_field(accu, *pc);
      Alloc_small(accu, Double_wosize, Double_tag);
      Store_double_val(accu, d);
      pc++;
      Next;
    }

    Instruct(SETFIELD0):
      caml_modify_field(accu, 0, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD1):
      caml_modify_field(accu, 1, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD2):
      caml_modify_field(accu, 2, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD3):
      caml_modify_field(accu, 3, *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD):
      caml_modify_field(accu, *pc, *sp++);
      accu = Val_unit;
      pc++;
      Next;
    Instruct(SETFLOATFIELD):
      Store_double_field(accu, *pc, Double_val(*sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;

/* Array operations */

    Instruct(VECTLENGTH): {
      mlsize_t size = Wosize_val(accu);
      if (Tag_val(accu) == Double_array_tag) size = size / Double_wosize;
      accu = Val_long(size);
      Next;
    }
    Instruct(GETVECTITEM):
      accu = Field(accu, Long_val(sp[0]));
      sp += 1;
      Next;
    Instruct(SETVECTITEM):
      caml_modify_field(accu, Long_val(sp[0]), sp[1]);
      accu = Val_unit;
      sp += 2;
      Next;

/* String operations */

    Instruct(GETSTRINGCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
      accu = Val_unit;
      Next;

/* Branches and conditional branches */

    Instruct(BRANCH):
      pc += *pc;
      Next;
    Instruct(BRANCHIF):
      if (accu != Val_false) pc += *pc; else pc++;
      Next;
    Instruct(BRANCHIFNOT):
      if (accu == Val_false) pc += *pc; else pc++;
      Next;
    Instruct(SWITCH): {
      uint32 sizes = *pc++;
      if (Is_block(accu)) {
        intnat index = Tag_val(accu);
        Assert ((uintnat) index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        intnat index = Long_val(accu);
        Assert ((uintnat) index < (sizes & 0xFFFF)) ;
        pc += pc[index];
      }
      Next;
    }
    Instruct(BOOLNOT):
      accu = Val_not(accu);
      Next;

/* Exceptions */

    Instruct(PUSHTRAP):
      sp -= 4;
      Trap_pc(sp) = Val_pc(pc + *pc);
      Trap_link(sp) = Val_long(caml_trap_sp_off);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      caml_trap_sp_off = sp - caml_stack_high;
      pc++;
      Next;

    Instruct(POPTRAP):
      if (caml_something_to_do) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      caml_trap_sp_off = Long_val(Trap_link(sp));
      sp += 4;
      Next;

    Instruct(RAISE_NOTRACE):
      if (caml_trap_sp_off >= caml_trap_barrier_off) caml_debugger(TRAP_BARRIER);
      goto raise_notrace;

    Instruct(RERAISE):
      if (caml_trap_sp_off >= caml_trap_barrier_off) caml_debugger(TRAP_BARRIER);
      if (caml_backtrace_active) caml_stash_backtrace(accu, pc, sp, 1);
      goto raise_notrace;

    Instruct(RAISE):
    raise_exception:
      if (caml_trap_sp_off >= caml_trap_barrier_off) caml_debugger(TRAP_BARRIER);
      if (caml_backtrace_active) caml_stash_backtrace(accu, pc, sp, 0);
    raise_notrace:
      if (caml_trap_sp_off > 0) {
        if (caml_parent_stack == Val_long(0)) {
          caml_external_raise = initial_external_raise;
          caml_parent_stack = initial_parent_stack;
          caml_trap_sp_off = initial_trap_sp_off;
          caml_extern_sp = caml_stack_high - initial_stack_words;
          caml_callback_depth--;
          return Make_exception_result(accu);
        } else {
          value cont;
          Setup_for_context_switch
          cont = caml_finish_exception(accu);
          Restore_after_context_switch
          extra_args = Long_val(sp[0]);
          sp += 1;
          pc = Code_val(cont);
          env = cont;
          goto check_stacks;
        }
      } else {
        sp = caml_stack_high + caml_trap_sp_off;
        pc = Pc_val(Trap_pc(sp));
        caml_trap_sp_off = Long_val(Trap_link(sp));
        env = sp[2];
        extra_args = Long_val(sp[3]);
        sp += 4;
      }
      Next;



/* Stack checks */

    check_stacks:
      if (sp < caml_stack_threshold) {
        value saved[] = {env, accu};
        caml_extern_sp = sp;
        caml_realloc_stack(Stack_threshold / sizeof(value), saved, 2);
        sp = caml_extern_sp;
        env = saved[0];
        accu = saved[1];
      }
      /* Fall through CHECK_SIGNALS */

/* Signal handling */

    Instruct(CHECK_SIGNALS):    /* accu not preserved */
      if (caml_something_to_do) goto process_signal;
      Next;

    process_signal:
      caml_something_to_do = 0;
      Setup_for_event;
      caml_process_event();
      Restore_after_event;
      Next;

/* Calling C functions */

    Instruct(C_CALL1):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu);
      Restore_after_c_call;
      pc++;
      Next;
    Instruct(C_CALL2):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[1], sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    Instruct(C_CALLN): {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = Primitive(*pc)(sp + 1, nargs);
      Restore_after_c_call;
      sp += nargs;
      pc++;
      Next;
    }

/* Integer constants */

    Instruct(CONST0):
      accu = Val_int(0); Next;
    Instruct(CONST1):
      accu = Val_int(1); Next;
    Instruct(CONST2):
      accu = Val_int(2); Next;
    Instruct(CONST3):
      accu = Val_int(3); Next;

    Instruct(PUSHCONST0):
      *--sp = accu; accu = Val_int(0); Next;
    Instruct(PUSHCONST1):
      *--sp = accu; accu = Val_int(1); Next;
    Instruct(PUSHCONST2):
      *--sp = accu; accu = Val_int(2); Next;
    Instruct(PUSHCONST3):
      *--sp = accu; accu = Val_int(3); Next;

    Instruct(PUSHCONSTINT):
      *--sp = accu;
      /* Fallthrough */
    Instruct(CONSTINT):
      accu = Val_int(*pc);
      pc++;
      Next;

/* Integer arithmetic */

    Instruct(NEGINT):
      accu = (value)(2 - (intnat)accu); Next;
    Instruct(ADDINT):
      accu = (value)((intnat) accu + (intnat) *sp++ - 1); Next;
    Instruct(SUBINT):
      accu = (value)((intnat) accu - (intnat) *sp++ + 1); Next;
    Instruct(MULINT):
      accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;

    Instruct(DIVINT): {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) / divisor);
      Next;
    }
    Instruct(MODINT): {
      intnat divisor = Long_val(*sp++);
      if (divisor == 0) { Setup_for_c_call; caml_raise_zero_divide(); }
      accu = Val_long(Long_val(accu) % divisor);
      Next;
    }
    Instruct(ANDINT):
      accu = (value)((intnat) accu & (intnat) *sp++); Next;
    Instruct(ORINT):
      accu = (value)((intnat) accu | (intnat) *sp++); Next;
    Instruct(XORINT):
      accu = (value)(((intnat) accu ^ (intnat) *sp++) | 1); Next;
    Instruct(LSLINT):
      accu = (value)((((intnat) accu - 1) << Long_val(*sp++)) + 1); Next;
    Instruct(LSRINT):
      accu = (value)((((uintnat) accu - 1) >> Long_val(*sp++)) | 1);
      Next;
    Instruct(ASRINT):
      accu = (value)((((intnat) accu - 1) >> Long_val(*sp++)) | 1); Next;

#define Integer_comparison(typ,opname,tst) \
    Instruct(opname): \
      accu = Val_int((typ) accu tst (typ) *sp++); Next;

    Integer_comparison(intnat,EQ, ==)
    Integer_comparison(intnat,NEQ, !=)
    Integer_comparison(intnat,LTINT, <)
    Integer_comparison(intnat,LEINT, <=)
    Integer_comparison(intnat,GTINT, >)
    Integer_comparison(intnat,GEINT, >=)
    Integer_comparison(uintnat,ULTINT, <)
    Integer_comparison(uintnat,UGEINT, >=)

#define Integer_branch_comparison(typ,opname,tst,debug) \
    Instruct(opname): \
      if ( *pc++ tst (typ) Long_val(accu)) { \
        pc += *pc ; \
      } else { \
        pc++ ; \
      } ; Next;

    Integer_branch_comparison(intnat,BEQ, ==, "==")
    Integer_branch_comparison(intnat,BNEQ, !=, "!=")
    Integer_branch_comparison(intnat,BLTINT, <, "<")
    Integer_branch_comparison(intnat,BLEINT, <=, "<=")
    Integer_branch_comparison(intnat,BGTINT, >, ">")
    Integer_branch_comparison(intnat,BGEINT, >=, ">=")
    Integer_branch_comparison(uintnat,BULTINT, <, "<")
    Integer_branch_comparison(uintnat,BUGEINT, >=, ">=")

    Instruct(OFFSETINT):
      accu += *pc << 1;
      pc++;
      Next;
    Instruct(OFFSETREF): {
        value v = Field(accu, 0);
        Assert(!Is_block(v));
        v += *pc << 1;
        Op_val(accu)[0] = v; /* ?? */
      }
      accu = Val_unit;
      pc++;
      Next;
    Instruct(ISINT):
      accu = Val_long(accu & 1);
      Next;

/* Object-oriented operations */

#define Lookup(obj, lab) Field (Field (obj, 0), Int_val(lab))

      /* please don't forget to keep below code in sync with the
         functions caml_cache_public_method and
         caml_cache_public_method2 in obj.c */

    Instruct(GETMETHOD):
      accu = Lookup(sp[0], accu);
      Next;

#define CAML_METHOD_CACHE
#ifdef CAML_METHOD_CACHE
    Instruct(GETPUBMET): {
      /* accu == object, pc[0] == tag, pc[1] == cache */
      value meths = Field (accu, 0);
      value ofs;
#ifdef CAML_TEST_CACHE
      static int calls = 0, hits = 0;
      if (calls >= 10000000) {
        fprintf(stderr, "cache hit = %d%%\n", hits / 100000);
        calls = 0; hits = 0;
      }
      calls++;
#endif
      *--sp = accu;
      accu = Val_int(*pc++);
      ofs = *pc & Field(meths,1);
      if (*(value*)(((char*)(Op_val(meths)+3)) + ofs) == accu) {
#ifdef CAML_TEST_CACHE
        hits++;
#endif
        accu = *(value*)(((char*)(Op_val(meths)+2)) + ofs);
      }
      else
      {
        int li = 3, hi = Field(meths,0), mi;
        while (li < hi) {
          mi = ((li+hi) >> 1) | 1;
          if (accu < Field(meths,mi)) hi = mi-2;
          else li = mi;
        }
        *pc = (li-3)*sizeof(value);
        accu = Field (meths, li-1);
      }
      pc++;
      Next;
    }
#else
    Instruct(GETPUBMET):
      *--sp = accu;
      accu = Val_int(*pc);
      pc += 2;
      /* Fallthrough */
#endif
    Instruct(GETDYNMET): {
      /* accu == tag, sp[0] == object, *pc == cache */
      value meths = Field (sp[0], 0);
      int li = 3, hi = Field(meths,0), mi;
      while (li < hi) {
        mi = ((li+hi) >> 1) | 1;
        if (accu < Field(meths,mi)) hi = mi-2;
        else li = mi;
      }
      accu = Field (meths, li-1);
      Next;
    }

/* Debugging and machine control */

    Instruct(STOP):
      caml_external_raise = initial_external_raise;
      caml_parent_stack = initial_parent_stack;
      caml_trap_sp_off = initial_trap_sp_off;
      caml_extern_sp = sp;
      caml_callback_depth--;
      return accu;

    Instruct(EVENT):
      if (--caml_event_count == 0) {
        Setup_for_debugger;
        caml_debugger(EVENT_COUNT);
        Restore_after_debugger;
      }
      Restart_curr_instr;

    Instruct(BREAK):
      Setup_for_debugger;
      caml_debugger(BREAKPOINT);
      Restore_after_debugger;
      Restart_curr_instr;

/* Context switching */

    Instruct(HANDLE): {
      value hval, hexn, heff;
      value cont;

      hval = sp[0];
      hexn = sp[1];
      heff = sp[2];
      sp += 3;

      Setup_for_context_switch
      cont = caml_handle(accu, hval, hexn, heff, 0);
      Restore_after_context_switch

      pc = Code_val(cont);
      env = cont;
      extra_args = 0;
      goto check_stacks;
    }

    Instruct(HANDLETERM): {
      value hval, hexn, heff;
      value cont;

      hval = sp[0];
      hexn = sp[1];
      heff = sp[2];

      Setup_for_tail_context_switch
      cont = caml_handle(accu, hval, hexn, heff, extra_args);
      Restore_after_context_switch

      pc = Code_val(cont);
      env = cont;
      extra_args = 0;
      goto check_stacks;
    }

    Instruct(PERFORM): {
      value cont;
      if (caml_parent_stack == Val_long(0)) {
        accu = Field(caml_read_root(caml_global_data), UNHANDLED_EXN);
        goto raise_exception;
      } else {
        Setup_for_context_switch
        cont = caml_perform(accu);
        Restore_after_context_switch

        extra_args = Long_val(sp[0]) + 1;
        sp += 1;

        pc = Code_val(cont);
        env = cont;
        goto check_stacks;
      }
    }

    Instruct(CONTINUE): {
      value ret;

      ret = sp[0];
      sp += 1;

      Setup_for_context_switch
      accu = caml_continue(accu, ret, 0);
      Restore_after_context_switch

      pc = Pc_val(sp[0]);
      env = sp[1];
      extra_args = Long_val(sp[2]);
      sp += 3;
      Next;
    }

    Instruct(CONTINUETERM): {
      value ret;

      ret = sp[0];

      Setup_for_tail_context_switch
      accu = caml_continue(accu, ret, extra_args);
      Restore_after_context_switch

      pc = Pc_val(sp[0]);
      env = sp[1];
      extra_args = Long_val(sp[2]);
      sp += 3;
      Next;
    }

    Instruct(DISCONTINUE): {
      value ret;

      ret = sp[0];
      sp += 1;

      Setup_for_context_switch
      accu = caml_continue(accu, ret, 0);
      Restore_after_context_switch

      goto raise_exception;
    }

    Instruct(DISCONTINUETERM): {
      value ret;

      ret = sp[0];

      Setup_for_tail_context_switch
      accu = caml_continue(accu, ret, extra_args);
      Restore_after_context_switch

      goto raise_exception;
    }

    Instruct(FINISH): {
      value cont;

      Setup_for_context_switch
      cont = caml_finish(accu);
      Restore_after_context_switch

      extra_args = Long_val(sp[0]);
      sp += 1;

      pc = Code_val(cont);
      env = cont;
      goto check_stacks;
    }

#ifndef THREADED_CODE
    default:
#if _MSC_VER >= 1200
      __assume(0);
#else
      caml_fatal_error_arg("Fatal error: bad opcode (%"
                           ARCH_INTNAT_PRINTF_FORMAT "x)\n",
                           (char *) (intnat) *(pc-1));
#endif
    }
  }
#endif
}

void caml_prepare_bytecode(code_t prog, asize_t prog_size) {
  /* other implementations of the interpreter (such as an hypothetical
     JIT translator) might want to do something with a bytecode before
     running it */
  Assert(prog);
  Assert(prog_size>0);
  /* actually, the threading of the bytecode might be done here */
}

void caml_release_bytecode(code_t prog, asize_t prog_size) {
  /* other implementations of the interpreter (such as an hypothetical
     JIT translator) might want to know when a bytecode is removed */
  /* check that we have a program */
  Assert(prog);
  Assert(prog_size>0);
}
