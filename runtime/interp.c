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

/* The bytecode interpreter */
#include <stdio.h>
#include "caml/alloc.h"
#include "caml/backtrace.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/debugger.h"
#include "caml/fail.h"
#include "caml/fix_code.h"
#include "caml/instrtrace.h"
#include "caml/instruct.h"
#include "caml/interp.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/prims.h"
#include "caml/signals.h"
#include "caml/fiber.h"
#include "caml/domain.h"
#include "caml/globroots.h"
#include "caml/startup.h"
#include "caml/startup_aux.h"

/* Registers for the abstract machine:
        pc         the code pointer
        sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
        Caml_state->trap_sp_off offset to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable Caml_state->extern_sp. */

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name
#  if defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
#    define Jumptbl_base &&lbl_ACC0
#  else
#    define Jumptbl_base 0
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
  { sp -= 3; sp[0] = accu; sp[1] = env; sp[2] = (value)pc; \
    domain_state->current_stack->sp = sp; }
#define Restore_after_gc \
  { sp = domain_state->current_stack->sp; accu = sp[0]; env = sp[1]; sp += 3; }
/* Do call asynchronous callbacks from allocation functions */
#define Enter_gc(dom_st, wosize) do {                            \
    Setup_for_gc;                                                \
    Alloc_small_enter_GC_flags(CAML_DO_TRACK | CAML_FROM_CAML,   \
                               dom_st, wosize);                  \
    Restore_after_gc;                                            \
  } while (0)

/* We store [pc+1] in the stack so that, in case of an exception, the
   first backtrace slot points to the event following the C call
   instruction. */
#define Setup_for_c_call \
  { sp -= 2; sp[0] = env; sp[1] = (value)(pc + 1); \
    domain_state->current_stack->sp = sp; }
#define Restore_after_c_call \
  { sp = domain_state->current_stack->sp; env = *sp; sp += 2; }

/* For VM threads purposes, an event frame must look like accu + a
   C_CALL frame + a RETURN 1 frame.
   TODO: now that VM threads are gone, we could get rid of that. But
   we need to make sure that this is not used elsewhere. */
#define Setup_for_event \
  { sp -= 6; \
    sp[0] = accu; /* accu */ \
    sp[1] = Val_unit; /* C_CALL frame: dummy environment */ \
    sp[2] = Val_unit; /* RETURN frame: dummy local 0 */ \
    sp[3] = (value) pc; /* RETURN frame: saved return address */  \
    sp[4] = env; /* RETURN frame: saved environment */ \
    sp[5] = Val_long(extra_args); /* RETURN frame: saved extra args */ \
    domain_state->current_stack->sp = sp; }
#define Restore_after_event \
  { sp = domain_state->current_stack->sp; accu = sp[0]; \
    pc = (code_t) sp[3]; env = sp[4]; extra_args = Long_val(sp[5]); \
    sp += 6; }

/* Debugger interface */

#define Setup_for_debugger \
   { sp -= 4; \
     sp[0] = accu; sp[1] = (value)(pc - 1); \
     sp[2] = env; sp[3] = Val_long(extra_args); \
     domain_state->current_stack->sp = sp; }
#define Restore_after_debugger \
   { CAMLassert(sp == domain_state->current_stack->sp); \
     CAMLassert(sp[0] == accu); \
     CAMLassert(sp[2] == env); \
     sp += 4; }

#ifdef THREADED_CODE
#define Restart_curr_instr \
  goto *((void*)(jumptbl_base + caml_debugger_saved_instruction(pc - 1)))
#else
#define Restart_curr_instr \
  curr_instr = caml_debugger_saved_instruction(pc - 1); \
  goto dispatch_instr
#endif

Caml_inline void check_trap_barrier_for_exception
  (caml_domain_state* domain_state)
{
  if (domain_state->current_stack->id == domain_state->trap_barrier_block
      && domain_state->trap_sp_off >= domain_state->trap_barrier_off)
    caml_debugger(TRAP_BARRIER, Val_unit);
}

Caml_inline void check_trap_barrier_for_effect
  (caml_domain_state* domain_state)
{
  if (domain_state->current_stack->id == domain_state->trap_barrier_block){
    caml_debugger(TRAP_BARRIER, Val_unit);
  }else{
    struct stack_info *parent_stack
      = domain_state->current_stack->handler->parent;
    if (parent_stack != NULL
        && parent_stack->id == domain_state->trap_barrier_block
        && parent_stack->sp + 2 - Stack_high (parent_stack)
              /* Note: +2 is the same constant as in debugger.c:552 */
           == domain_state->trap_barrier_off){
      caml_debugger(TRAP_BARRIER, Val_unit);
    }
  }
}

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

static value raise_unhandled_effect;

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
  intnat initial_trap_sp_off;
  volatile value raise_exn_bucket = Val_unit;
  struct longjmp_buffer raise_buf;
  value resume_fn, resume_arg;
  caml_domain_state* domain_state = Caml_state;
  struct caml_exception_context exception_ctx =
    { &raise_buf, domain_state->local_roots, &raise_exn_bucket};
#ifndef THREADED_CODE
  opcode_t curr_instr;
#endif

#ifdef THREADED_CODE
  static void * jumptable[] = {
#    include "caml/jumptbl.h"
  };
#endif

  if (prog == NULL) {           /* Interpreter is initializing */
    static opcode_t raise_unhandled_effect_code[] = { ACC, 0, RAISE };
    value raise_unhandled_effect_closure;

    caml_register_code_fragment(
      (char *) raise_unhandled_effect_code,
      (char *) raise_unhandled_effect_code +
      sizeof(raise_unhandled_effect_code),
      DIGEST_IGNORE, NULL);
#ifdef THREADED_CODE
    caml_init_thread_code(jumptable, Jumptbl_base);
    caml_thread_code(raise_unhandled_effect_code,
                     sizeof(raise_unhandled_effect_code));
#endif
    raise_unhandled_effect_closure = caml_alloc_small (2, Closure_tag);
    Code_val(raise_unhandled_effect_closure) =
      (code_t)raise_unhandled_effect_code;
    Closinfo_val(raise_unhandled_effect_closure) = Make_closinfo(0, 2);
    raise_unhandled_effect = raise_unhandled_effect_closure;
    caml_register_generational_global_root(&raise_unhandled_effect);
    caml_global_data = Val_unit;
    caml_register_generational_global_root(&caml_global_data);
    caml_init_callbacks();
    return Val_unit;
  }

#if defined(THREADED_CODE) && defined(ARCH_SIXTYFOUR) && !defined(ARCH_CODE32)
  jumptbl_base = Jumptbl_base;
#endif
  initial_trap_sp_off = domain_state->trap_sp_off;
  initial_stack_words =
    Stack_high(domain_state->current_stack) - domain_state->current_stack->sp;
  initial_external_raise = domain_state->external_raise;

  if (sigsetjmp(raise_buf.buf, 0)) {
    /* no non-volatile local variables read here */
    sp = domain_state->current_stack->sp;
    accu = raise_exn_bucket;

    check_trap_barrier_for_exception (domain_state);
    if (domain_state->backtrace_active) {
         /* pc has already been pushed on the stack when calling the C
         function that raised the exception. No need to push it again
         here. */
      caml_stash_backtrace(accu, sp, 0);
    }
    goto raise_notrace;
  }
  domain_state->external_raise = &exception_ctx;

  domain_state->trap_sp_off = 1;

  sp = domain_state->current_stack->sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);

#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (caml_icount-- == 0) caml_stop_here ();
  CAMLassert(Stack_base(domain_state->current_stack) <= sp);
  CAMLassert(sp <= Stack_high(domain_state->current_stack));
#endif
  goto *(void *)(jumptbl_base + *pc++); /* Jump to the first instruction */
#else
  while(1) {
#ifdef DEBUG
    caml_bcodcount++;
    if (caml_icount-- == 0) caml_stop_here ();
    if (caml_params->trace_level>1)
      printf("\n##%" ARCH_INTNAT_PRINTF_FORMAT "d\n", caml_bcodcount);
    if (caml_params->trace_level>0) caml_disasm_instr(pc);
    if (caml_params->trace_level>1) {
      printf("env=");
      caml_trace_value_file(env,prog,prog_size,stdout);
      putchar('\n');
      caml_trace_accu_sp_file(accu,sp,prog,prog_size,stdout);
      fflush(stdout);
    };
    CAMLassert(Stack_base(domain_state->current_stack) <= sp);
    CAMLassert(sp <= Stack_high(domain_state->current_stack));

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
      accu = Field(env, 1); Next;
    Instruct(ENVACC2):
      accu = Field(env, 2); Next;
    Instruct(ENVACC3):
      accu = Field(env, 3); Next;
    Instruct(ENVACC4):
      accu = Field(env, 4); Next;

    Instruct(PUSHENVACC1):
      *--sp = accu; accu = Field(env, 1); Next;
    Instruct(PUSHENVACC2):
      *--sp = accu; accu = Field(env, 2); Next;
    Instruct(PUSHENVACC3):
      *--sp = accu; accu = Field(env, 3); Next;
    Instruct(PUSHENVACC4):
      *--sp = accu; accu = Field(env, 4); Next;

    Instruct(PUSHENVACC):
      *--sp = accu;
      /* Fallthrough */
    Instruct(ENVACC):
      accu = Field(env, *pc++);
      Next;

/* Function application */

    Instruct(PUSH_RETADDR): {
      sp -= 3;
      sp[0] = (value) (pc + *pc);
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
      sp[1] = (value)pc;
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
      sp[2] = (value)pc;
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
      sp[3] = (value)pc;
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
        Next;
      } else {
        goto do_return;
      }
    }

    do_return:
      if (sp == Stack_high(domain_state->current_stack)) {
        /* return to parent stack */
        struct stack_info* old_stack = domain_state->current_stack;
        struct stack_info* parent_stack = Stack_parent(old_stack);
        value hval = Stack_handle_value(old_stack);
        CAMLassert(parent_stack != NULL);

        domain_state->current_stack = parent_stack;
        sp = domain_state->current_stack->sp;
        caml_free_stack(old_stack);

        domain_state->trap_sp_off = Long_val(sp[0]);
        extra_args = Long_val(sp[1]);
        sp++;
        sp[0] = accu;

        accu = hval;
        pc = Code_val(accu);
        env = accu;
        goto check_stacks;
      } else {
        /* return to callee, no stack switching */
        pc = (code_t)(sp[0]);
        env = sp[1];
        extra_args = Long_val(sp[2]);
        sp += 3;
      }
      Next;

    Instruct(RESTART): {
      int num_args = Wosize_val(env) - 3;
      int i;
      sp -= num_args;
      for (i = 0; i < num_args; i++) sp[i] = Field(env, i + 3);
      env = Field(env, 2);
      extra_args += num_args;
      Next;
    }

    Instruct(GRAB): {
      int required = *pc++;
      if (extra_args >= required) {
        extra_args -= required;
        Next;
      } else {
        mlsize_t num_args, i;
        num_args = 1 + extra_args; /* arg1 + extra args */
        Alloc_small(accu, num_args + 3, Closure_tag, Enter_gc);
        Field(accu, 2) = env;
        for (i = 0; i < num_args; i++) Field(accu, i + 3) = sp[i];
        Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
        Closinfo_val(accu) = Make_closinfo(0, 2);
        sp += num_args;
        goto do_return;
      }
    }

    Instruct(CLOSURE): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      if (nvars <= Max_young_wosize - 2) {
        /* nvars + 2 <= Max_young_wosize, can allocate in minor heap */
        Alloc_small(accu, 2 + nvars, Closure_tag, Enter_gc);
        for (i = 0; i < nvars; i++) Field(accu, i + 2) = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(2 + nvars, Closure_tag);
        for (i = 0; i < nvars; i++) caml_initialize(&Field(accu, i + 2), sp[i]);
      }
      /* The code pointer is not in the heap, so no need to go through
         caml_initialize. */
      Code_val(accu) = pc + *pc;
      Closinfo_val(accu) = Make_closinfo(0, 2);
      pc++;
      sp += nvars;
      Next;
    }

    Instruct(CLOSUREREC): {
      int nfuncs = *pc++;
      int nvars = *pc++;
      mlsize_t envofs = nfuncs * 3 - 1;
      mlsize_t blksize = envofs + nvars;
      int i;
      volatile value * p;
      if (nvars > 0) *--sp = accu;
      if (blksize <= Max_young_wosize) {
        Alloc_small(accu, blksize, Closure_tag, Enter_gc);
        p = &Field(accu, envofs);
        for (i = 0; i < nvars; i++, p++) *p = sp[i];
      } else {
        /* PR#6385: must allocate in major heap */
        /* caml_alloc_shr and caml_initialize never trigger a GC,
           so no need to Setup_for_gc */
        accu = caml_alloc_shr(blksize, Closure_tag);
        p = &Field(accu, envofs);
        for (i = 0; i < nvars; i++, p++) caml_initialize(p, sp[i]);
      }
      sp += nvars;
      /* The code pointers and infix headers are not in the heap,
         so no need to go through caml_initialize. */
      *--sp = accu;
      p = &Field(accu, 0);
      *p++ = (value) (pc + pc[0]);
      *p++ = Make_closinfo(0, envofs);
      for (i = 1; i < nfuncs; i++) {
        *p++ = Make_header(i * 3, Infix_tag, 0); /* color irrelevant */
        *--sp = (value) p;
        *p++ = (value) (pc + pc[i]);
        envofs -= 3;
        *p++ = Make_closinfo(0, envofs);
      }
      pc += nfuncs;
      Next;
    }

    Instruct(PUSHOFFSETCLOSURE):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE):
      accu = env + *pc++ * sizeof(value); Next;

    Instruct(PUSHOFFSETCLOSUREM3):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSUREM3):
      accu = env - 3 * sizeof(value); Next;
    Instruct(PUSHOFFSETCLOSURE0):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE0):
      accu = env; Next;
    Instruct(PUSHOFFSETCLOSURE3):
      *--sp = accu; /* fallthrough */
    Instruct(OFFSETCLOSURE3):
      accu = env + 3 * sizeof(value); Next;


/* Access to global variables */

    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field(caml_global_data, *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field(caml_global_data, *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL):  {
      caml_modify(&Field(caml_global_data, *pc), accu);
      accu = Val_unit;
      pc++;
      Next;
    }

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
        Alloc_small(block, wosize, tag, Enter_gc);
        Field(block, 0) = accu;
        for (i = 1; i < wosize; i++) Field(block, i) = *sp++;
      } else {
        block = caml_alloc_shr(wosize, tag);
        caml_initialize(&Field(block, 0), accu);
        for (i = 1; i < wosize; i++) caml_initialize(&Field(block, i), *sp++);
      }
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag, Enter_gc);
      Field(block, 0) = accu;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag, Enter_gc);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag, Enter_gc);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      Field(block, 2) = sp[1];
      sp += 2;
      accu = block;
      Next;
    }
    Instruct(MAKEFLOATBLOCK): {
      mlsize_t size = *pc++;
      mlsize_t i;
      value block;
      if (size <= Max_young_wosize / Double_wosize) {
        Alloc_small(block, size * Double_wosize, Double_array_tag, Enter_gc);
      } else {
        block = caml_alloc_shr(size * Double_wosize, Double_array_tag);
      }
      Store_double_flat_field(block, 0, Double_val(accu));
      for (i = 1; i < size; i++){
        Store_double_flat_field(block, i, Double_val(*sp));
        ++ sp;
      }
      accu = block;
      Next;
    }

/* Access to components of blocks */

    Instruct(GETFIELD0):
      accu = Field(accu, 0); Next;
    Instruct(GETFIELD1):
      accu = Field(accu, 1); Next;
    Instruct(GETFIELD2):
      accu = Field(accu, 2); Next;
    Instruct(GETFIELD3):
      accu = Field(accu, 3); Next;
    Instruct(GETFIELD):
      accu = Field(accu, *pc); pc++; Next;
    Instruct(GETFLOATFIELD): {
      double d = Double_flat_field(accu, *pc++);
      Alloc_small(accu, Double_wosize, Double_tag, Enter_gc);
      Store_double_val(accu, d);
      Next;
    }

    Instruct(SETFIELD0):
      caml_modify(&Field(accu, 0), *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD1):
      caml_modify(&Field(accu, 1), *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD2):
      caml_modify(&Field(accu, 2), *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD3):
      caml_modify(&Field(accu, 3), *sp++);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD):
      caml_modify(&Field(accu, *pc), *sp++);
      accu = Val_unit;
      pc++;
      Next;
    Instruct(SETFLOATFIELD):
      Store_double_flat_field(accu, *pc, Double_val(*sp));
      accu = Val_unit;
      sp++;
      pc++;
      Next;

/* Array operations */

    Instruct(VECTLENGTH): {
      /* Todo: when FLAT_FLOAT_ARRAY is false, this instruction should
         be split into VECTLENGTH and FLOATVECTLENGTH because we know
         statically which one it is. */
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
      caml_modify(&Field(accu, Long_val(sp[0])), sp[1]);
      accu = Val_unit;
      sp += 2;
      Next;

/* Bytes/String operations */
    Instruct(GETSTRINGCHAR):
    Instruct(GETBYTESCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    Instruct(SETBYTESCHAR):
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
      uint32_t sizes = *pc++;
      if (Is_block(accu)) {
        intnat index = Tag_val(accu);
        CAMLassert ((uintnat) index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        intnat index = Long_val(accu);
        CAMLassert ((uintnat) index < (sizes & 0xFFFF)) ;
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
      Trap_pc(sp) = pc + *pc;
      Trap_link(sp) = Val_long(domain_state->trap_sp_off);
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      domain_state->trap_sp_off = sp - Stack_high(domain_state->current_stack);
      pc++;
      Next;

    Instruct(POPTRAP):
      if (Caml_check_gc_interrupt(domain_state)) {
        /* We must check here so that if a signal is pending and its
           handler triggers an exception, the exception is trapped
           by the current try...with, not the enclosing one. */
        pc--; /* restart the POPTRAP after processing the signal */
        goto process_signal;
      }
      domain_state->trap_sp_off = Long_val(Trap_link(sp));
      sp += 4;
      Next;

    Instruct(RAISE_NOTRACE):
      check_trap_barrier_for_exception (domain_state);
      goto raise_notrace;

    Instruct(RERAISE):
      check_trap_barrier_for_exception (domain_state);
      if (domain_state->backtrace_active) {
        *--sp = (value)(pc - 1);
        caml_stash_backtrace(accu, sp, 1);
      }
      goto raise_notrace;

    Instruct(RAISE):
    raise_exception:
      check_trap_barrier_for_exception (domain_state);
      if (domain_state->backtrace_active) {
        *--sp = (value)(pc - 1);
        caml_stash_backtrace(accu, sp, 0);
      }
    raise_notrace:
      if (domain_state->trap_sp_off > 0) {
        if (Stack_parent(domain_state->current_stack) == NULL) {
          domain_state->external_raise = initial_external_raise;
          domain_state->trap_sp_off = initial_trap_sp_off;
          domain_state->current_stack->sp =
            Stack_high(domain_state->current_stack) - initial_stack_words ;
          return Make_exception_result(accu);
        } else {
          struct stack_info* old_stack = domain_state->current_stack;
          struct stack_info* parent_stack = Stack_parent(old_stack);
          value hexn = Stack_handle_exception(old_stack);
          old_stack->sp = sp;
          domain_state->current_stack = parent_stack;
          sp = domain_state->current_stack->sp;
          caml_free_stack(old_stack);

          domain_state->trap_sp_off = Long_val(sp[0]);
          extra_args = Long_val(sp[1]);
          sp++;
          sp[0] = accu;

          accu = hexn;
          pc = Code_val(accu);
          env = accu;
          goto check_stacks;
        }
      } else {
        sp =
           Stack_high(domain_state->current_stack) + domain_state->trap_sp_off;
        pc = Trap_pc(sp);
        domain_state->trap_sp_off = Long_val(Trap_link(sp));
        env = sp[2];
        extra_args = Long_val(sp[3]);
        sp += 4;
      }
      Next;

/* Stack checks */

    check_stacks:
      if (sp < Stack_threshold_ptr(domain_state->current_stack)) {
        domain_state->current_stack->sp = sp;
        if (!caml_try_realloc_stack(Stack_threshold / sizeof(value))) {
          Setup_for_c_call; caml_raise_stack_overflow();
        }
        sp = domain_state->current_stack->sp;
      }
      /* Fall through CHECK_SIGNALS */

/* Signal handling */

    Instruct(CHECK_SIGNALS):    /* accu not preserved */
      if (Caml_check_gc_interrupt(domain_state))
        goto process_signal;
      Next;

    process_signal:
      Setup_for_event;
      caml_process_pending_actions();
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
      accu = Primitive(*pc)(accu, sp[2]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[2], sp[3]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = Primitive(*pc)(accu, sp[2], sp[3], sp[4], sp[5]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    Instruct(C_CALLN): {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = Primitive(*pc)(sp + 2, nargs);
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
      accu = (value)((((uintnat) accu) >> Long_val(*sp++)) | 1); Next;
    Instruct(ASRINT):
      accu = (value)((((intnat) accu) >> Long_val(*sp++)) | 1); Next;

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
    Instruct(OFFSETREF):
      Field(accu, 0) += *pc << 1;
      accu = Val_unit;
      pc++;
      Next;
    Instruct(ISINT):
      accu = Val_long(accu & 1);
      Next;

/* Object-oriented operations */

#define Lookup(obj, lab) Field (Field (obj, 0), Int_val(lab))

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
      /* We use relaxed atomic accesses to avoid racing with other domains
         updating the cache */
      ofs = atomic_load_explicit((_Atomic opcode_t *)pc, memory_order_relaxed)
            & Field(meths,1);
      if (*(value*)(((char*)&Field(meths,3)) + ofs) == accu) {
#ifdef CAML_TEST_CACHE
        hits++;
#endif
        accu = *(value*)(((char*)&Field(meths,2)) + ofs);
      }
      else
      {
        int li = 3, hi = Field(meths,0), mi;
        while (li < hi) {
          mi = ((li+hi) >> 1) | 1;
          if (accu < Field(meths,mi)) hi = mi-2;
          else li = mi;
        }
        atomic_store_explicit((_Atomic opcode_t *)pc, (li-3)*sizeof(value),
                              memory_order_relaxed);
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
      domain_state->external_raise = initial_external_raise;
      domain_state->trap_sp_off = initial_trap_sp_off;
      domain_state->current_stack->sp = sp;
      return accu;

    Instruct(EVENT):
      if (--caml_event_count == 0) {
        Setup_for_debugger;
        caml_debugger(EVENT_COUNT, Val_unit);
        Restore_after_debugger;
      }
      Restart_curr_instr;

    Instruct(BREAK):
      Setup_for_debugger;
      caml_debugger(BREAKPOINT, Val_unit);
      Restore_after_debugger;
      Restart_curr_instr;

/* Context switching */

    Instruct(RESUME):
      resume_fn = sp[0];
      resume_arg = sp[1];
      sp -= 3;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(0);
      sp[2] = (value)pc;
      sp[3] = env;
      sp[4] = Val_long(extra_args);
      goto do_resume;

do_resume: {
      struct stack_info* stk = Ptr_val(accu);
      if (stk == NULL) {
        Setup_for_c_call;
        caml_raise_continuation_already_resumed();
      }
      while (Stack_parent(stk) != NULL) stk = Stack_parent(stk);
      Stack_parent(stk) = Caml_state->current_stack;

      domain_state->current_stack->sp = sp;
      domain_state->current_stack = Ptr_val(accu);
      sp = domain_state->current_stack->sp;

      domain_state->trap_sp_off = Long_val(sp[0]);
      sp[0] = resume_arg;
      accu = resume_fn;
      pc = Code_val(accu);
      env = accu;
      extra_args = 0;
      goto check_stacks;
    }

    Instruct(RESUMETERM):
      resume_fn = sp[0];
      resume_arg = sp[1];
      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);
      goto do_resume;


    Instruct(PERFORM): {
      value cont;
      struct stack_info* old_stack = domain_state->current_stack;
      struct stack_info* parent_stack = Stack_parent(old_stack);

      check_trap_barrier_for_effect (domain_state);
      if (parent_stack == NULL) {
        Setup_for_c_call;
        accu = caml_make_unhandled_effect_exn(accu);
        Restore_after_c_call;
        goto raise_exception;
      }

      Alloc_small(cont, 1, Cont_tag, Enter_gc);

      sp -= 4;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = (value)pc;
      sp[2] = env;
      sp[3] = Val_long(extra_args);

      old_stack->sp = sp;
      domain_state->current_stack = parent_stack;
      sp = parent_stack->sp;
      Stack_parent(old_stack) = NULL;
      Field(cont, 0) = Val_ptr(old_stack);

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp--;
      sp[0] = accu;
      sp[1] = cont;
      sp[2] = Val_ptr(old_stack);
      accu = Stack_handle_effect(old_stack);
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

    Instruct(REPERFORMTERM): {
      value eff = accu;
      value cont = sp[0];
      struct stack_info* cont_tail = Ptr_val(sp[1]);
      struct stack_info* self = domain_state->current_stack;
      struct stack_info* parent = Stack_parent(domain_state->current_stack);

      check_trap_barrier_for_effect (domain_state);
      sp = sp + *pc - 2;
      sp[0] = Val_long(domain_state->trap_sp_off);
      sp[1] = Val_long(extra_args);

      if (parent == NULL) {
        Setup_for_c_call;
        resume_arg = caml_make_unhandled_effect_exn(eff);
        accu = caml_continuation_use(cont);
        Restore_after_c_call;
        resume_fn = raise_unhandled_effect;

        goto do_resume;
      }

      self->sp = sp;
      domain_state->current_stack = parent;
      sp = parent->sp;

      CAMLassert(Stack_parent(cont_tail) == NULL);
      Stack_parent(self) = NULL;
      Stack_parent(cont_tail) = self;

      domain_state->trap_sp_off = Long_val(sp[0]);
      extra_args = Long_val(sp[1]);
      sp--;
      sp[0] = eff;
      sp[1] = cont;
      sp[2] = Val_ptr(self);
      accu = Stack_handle_effect(self);
      pc = Code_val(accu);
      env = accu;
      extra_args += 2;
      goto check_stacks;
    }

#ifndef THREADED_CODE
    default:
#ifdef _MSC_VER
      __assume(0);
#else
      caml_fatal_error("bad opcode (%"
                           ARCH_INTNAT_PRINTF_FORMAT "x)",
                           (intnat) *(pc-1));
#endif
    }
  }
#endif
}
