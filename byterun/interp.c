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

/* The bytecode interpreter */

#include "alloc.h"
#include "fail.h"
#include "fix_code.h"
#include "instruct.h"
#include "interp.h"
#include "memory.h"
#include "misc.h"
#include "mlvalues.h"
#include "prims.h"
#include "signals.h"
#include "stacks.h"
#include "str.h"
#include "instrtrace.h"

/* Registers for the abstract machine:
	pc         the code pointer
	sp         the stack pointer (grows downward)
        accu       the accumulator
        env        heap-allocated environment
	trapsp     pointer to the current trap frame
        extra_args number of extra arguments provided by the caller

sp is a local copy of the global variable extern_sp. */

extern code_t start_code;
int callback_depth = 0;

/* Instruction decoding */

#ifdef THREADED_CODE
#  define Instruct(name) lbl_##name
#  ifdef DEBUG
#    define Next goto next_instr
#  else
#    define Next goto *((void *)((unsigned long)(*pc++)))
#  endif
#else
#  define Instruct(name) case name
#  define Next break
#endif

/* GC interface */

#define Setup_for_gc { sp -= 2; sp[0] = accu; sp[1] = env; extern_sp = sp; }
#define Restore_after_gc { accu = sp[0]; env = sp[1]; sp += 2; }
#define Setup_for_c_call { *--sp = env; extern_sp = sp; }
#define Restore_after_c_call { sp = extern_sp; env = *sp++; }

/* Register optimization.
   Many compilers underestimate the use of the local variables representing
   the abstract machine registers, and don't put them in hardware registers,
   which slows down the interpreter considerably.
   For GCC, I have hand-assigned hardware registers for several architectures.
*/

#if defined(__GNUC__) && !defined(DEBUG)
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
#define PC_REG asm("$9")
#define SP_REG asm("$10")
#define ACCU_REG asm("$11")
#endif
#ifdef __i386__
#define PC_REG asm("%esi")
#define SP_REG asm("%edi")
#define ACCU_REG
#endif
#if defined(PPC) || defined(_POWER) || defined(_IBMR2)
#define PC_REG asm("26")
#define SP_REG asm("27")
#define ACCU_REG asm("28")
#endif
#endif

/* The interpreter itself */

value interprete(prog, prog_size)
     code_t prog;
     asize_t prog_size;
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
  value env;
  long extra_args;
  struct longjmp_buffer * initial_external_raise;
  int initial_sp_offset;
  value * initial_local_roots;
  int initial_callback_depth;
  struct longjmp_buffer raise_buf;
  value * modify_dest, modify_newval;

#ifdef THREADED_CODE
  static void * jumptable[] = {
#    include "jumptbl.h"
  };
#endif

#ifdef THREADED_CODE
  if (prog[0] <= STOP) {
    instr_table = jumptable; 
    thread_code(prog, prog_size);
  }
#endif

  sp = extern_sp;
  pc = prog;
  extra_args = 0;
  env = Atom(0);
  accu = Val_int(0);
  initial_local_roots = local_roots;
  initial_sp_offset = stack_high - sp;
  initial_external_raise = external_raise;
  initial_callback_depth = callback_depth;
  if (sigsetjmp(raise_buf.buf, 1)) {
    local_roots = initial_local_roots;
    callback_depth = initial_callback_depth;
    accu = exn_bucket;
    goto raise_exception;
  }
  external_raise = &raise_buf;

#ifdef THREADED_CODE
#ifdef DEBUG
 next_instr:
  if (icount-- == 0) stop_here ();
  Assert(sp >= stack_low);
  Assert(sp <= stack_high);
  goto *((void *)((unsigned long)(*pc++)));
#else
  Next;                         /* Jump to the first instruction */
#endif
#else
  while(1) {
#ifdef DEBUG
    if (icount-- == 0) stop_here ();
    if (trace_flag) disasm_instr(pc);
    Assert(sp >= stack_low);
    Assert(sp <= stack_high);
#endif
    switch(*pc++) {
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
      extra_args = *pc++ - 1;
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
      int slotsize = *pc++;
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
      sp = sp + *pc++ - 1;
      sp[0] = arg1;
      pc = Code_val(accu);
      env = accu;
      goto check_stacks;
    }
    Instruct(APPTERM2): {
      value arg1 = sp[0];
      value arg2 = sp[1];
      sp = sp + *pc++ - 2;
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
      sp = sp + *pc++ - 3;
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
        pc = (code_t)(sp[0]);
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
        Field(accu, 1) = env;
        for (i = 0; i < num_args; i++) Field(accu, i + 2) = sp[i];
        Code_val(accu) = pc - 3; /* Point to the preceding RESTART instr. */
        sp += num_args;
        pc = (code_t)(sp[0]);
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
      Alloc_small(accu, 1 + nvars, Closure_tag);
      Code_val(accu) = pc + *pc;
      for (i = 0; i < nvars; i++) Field(accu, i + 1) = sp[i];
      sp += nvars;
      pc++;
      Next;
    }

    Instruct(CLOSUREREC): {
      int nvars = *pc++;
      int i;
      if (nvars > 0) *--sp = accu;
      Alloc_small(accu, 2 + nvars, Closure_tag);
      Code_val(accu) = pc + *pc;
      Field(accu, 1) = Val_int(0);
      for (i = 0; i < nvars; i++) Field(accu, i + 2) = sp[i];
      sp += nvars;
      modify(&Field(accu, 1), accu);
      pc++;
      Next;
    }

    Instruct(PUSHGETGLOBAL):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBAL):
      accu = Field(global_data, *pc);
      pc++;
      Next;

    Instruct(PUSHGETGLOBALFIELD):
      *--sp = accu;
      /* Fallthrough */
    Instruct(GETGLOBALFIELD): {
      accu = Field(global_data, *pc);
      pc++;
      accu = Field(accu, *pc);
      pc++;
      Next;
    }

    Instruct(SETGLOBAL):
      modify(&Field(global_data, *pc), accu);
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
      Alloc_small(block, wosize, tag);
      Field(block, 0) = accu;
      for (i = 1; i < wosize; i++) Field(block, i) = *sp++;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK1): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 1, tag);
      Field(block, 0) = accu;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK2): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 2, tag);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      sp += 1;
      accu = block;
      Next;
    }
    Instruct(MAKEBLOCK3): {
      tag_t tag = *pc++;
      value block;
      Alloc_small(block, 3, tag);
      Field(block, 0) = accu;
      Field(block, 1) = sp[0];
      Field(block, 2) = sp[1];
      sp += 2;
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

    Instruct(SETFIELD0):
      modify_dest = &Field(accu, 0);
      modify_newval = *sp++;
    modify:
      Modify(modify_dest, modify_newval);
      accu = Val_unit;
      Next;
    Instruct(SETFIELD1):
      modify_dest = &Field(accu, 1);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD2):
      modify_dest = &Field(accu, 2);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD3):
      modify_dest = &Field(accu, 3);
      modify_newval = *sp++;
      goto modify;
    Instruct(SETFIELD):
      modify_dest = &Field(accu, *pc);
      pc++;
      modify_newval = *sp++;
      goto modify;

/* For recursive definitions */

    Instruct(DUMMY): {
      int size = *pc++;
      Alloc_small(accu, size, 0);
      while (size--) Field(accu, size) = Val_long(0);
      Next;
    }
    Instruct(UPDATE): {
      value newval = *sp++;
      mlsize_t size, n;
      size = Wosize_val(newval);
      Assert(size == Wosize_val(accu));
      Tag_val(accu) = Tag_val(newval);
      for (n = 0; n < size; n++) {
        modify(&Field(accu, n), Field(newval, n));
      }
      accu = Val_unit;
      Next;
    }

/* Array operations */

    Instruct(VECTLENGTH):
      accu = Val_long(Wosize_val(accu));
      Next;
    Instruct(GETVECTITEM):
      accu = Field(accu, Long_val(sp[0]));
      sp += 1;
      Next;
    Instruct(SETVECTITEM):
      modify_dest = &Field(accu, Long_val(sp[0]));
      modify_newval = sp[1];
      sp += 2;
      goto modify;

/* String operations */

    Instruct(GETSTRINGCHAR):
      accu = Val_int(Byte_u(accu, Long_val(sp[0])));
      sp += 1;
      Next;
    Instruct(SETSTRINGCHAR):
      Byte_u(accu, Long_val(sp[0])) = Int_val(sp[1]);
      sp += 2;
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
        long index = Tag_val(accu);
        Assert(index >= 0 && index < (sizes >> 16));
        pc += pc[(sizes & 0xFFFF) + index];
      } else {
        long index = Long_val(accu);
        if ((unsigned long) index < (sizes & 0xFFFF))
          pc += pc[index];
        else
          pc += (sizes & 0xFFFF) + (sizes >> 16);
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
      Trap_link(sp) = trapsp;
      sp[2] = env;
      sp[3] = Val_long(extra_args);
      trapsp = sp;
      pc++;
      Next;

    Instruct(POPTRAP):
      /* We should check here if a signal is pending, to preserve the
         semantics of the program w.r.t. exceptions. Unfortunately,
         process_signal destroys the accumulator, and there is no
         convenient way to preserve it... */
      trapsp = Trap_link(sp);
      sp += 4;
      Next;

    Instruct(RAISE):            /* arg */
    raise_exception:
      sp = trapsp;
      if (sp >= stack_high - initial_sp_offset) {
        exn_bucket = accu;
        external_raise = initial_external_raise;
        siglongjmp(external_raise->buf, 1);
      }
      pc = Trap_pc(sp);
      trapsp = Trap_link(sp);
      env = sp[2];
      extra_args = Long_val(sp[3]);
      sp += 4;
      Next;

/* Stack checks */

    check_stacks:
      if (sp < stack_threshold) {
        extern_sp = sp;
        realloc_stack();
        sp = extern_sp;
      }
      /* Fall through CHECK_SIGNALS */

/* Signal handling */

    Instruct(CHECK_SIGNALS):    /* accu not preserved */
      if (something_to_do) goto process_signal;
      Next;

    process_signal:
      something_to_do = 0;
      if (force_major_slice){
	force_major_slice = 0;
	Setup_for_gc;
	minor_collection ();
	Restore_after_gc;
      }
      /* If a signal arrives between the following two instructions,
         it will be lost. */
      { int signal_number = pending_signal;
        pending_signal = 0;
        if (signal_number) {
          /* Push a return frame to the current code location */
          sp -= 4;
          sp[0] = Val_int(signal_number);
          sp[1] = (value) pc;
          sp[2] = env;
          sp[3] = Val_long(extra_args);
          /* Branch to the signal handler */
          env = Field(signal_handlers, signal_number);
          pc = Code_val(env);
          extra_args = 0;
        }
      }
      Next;

/* Calling C functions */

    Instruct(C_CALL1):
      Setup_for_c_call;
      accu = cprim[*pc](accu);
      Restore_after_c_call;
      pc++;
      Next;
    Instruct(C_CALL2):
      Setup_for_c_call;
      accu = cprim[*pc](accu, sp[1]);
      Restore_after_c_call;
      sp += 1;
      pc++;
      Next;
    Instruct(C_CALL3):
      Setup_for_c_call;
      accu = cprim[*pc](accu, sp[1], sp[2]);
      Restore_after_c_call;
      sp += 2;
      pc++;
      Next;
    Instruct(C_CALL4):
      Setup_for_c_call;
      accu = cprim[*pc](accu, sp[1], sp[2], sp[3]);
      Restore_after_c_call;
      sp += 3;
      pc++;
      Next;
    Instruct(C_CALL5):
      Setup_for_c_call;
      accu = cprim[*pc](accu, sp[1], sp[2], sp[3], sp[4]);
      Restore_after_c_call;
      sp += 4;
      pc++;
      Next;
    Instruct(C_CALLN): {
      int nargs = *pc++;
      *--sp = accu;
      Setup_for_c_call;
      accu = cprim[*pc](sp + 1, nargs);
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
      accu = (value)(2 - (long)accu); Next;
    Instruct(ADDINT):
      accu = (value)((long) accu + (long) *sp++ - 1); Next;
    Instruct(SUBINT):
      accu = (value)((long) accu - (long) *sp++ + 1); Next;
    Instruct(MULINT):
      accu = Val_long(Long_val(accu) * Long_val(*sp++)); Next;
    Instruct(DIVINT): {
      value div = *sp++;
      if (div == Val_long(0)) { Setup_for_c_call; raise_zero_divide(); }
      accu = Val_long(Long_val(accu) / Long_val(div));
      Next;
    }
    Instruct(MODINT): {
      value div = *sp++;
      if (div == Val_long(0)) { Setup_for_c_call; raise_zero_divide(); }
      accu = Val_long(Long_val(accu) % Long_val(div));
      Next;
    }
    Instruct(ANDINT):
      accu = (value)((long) accu & (long) *sp++); Next;
    Instruct(ORINT):
      accu = (value)((long) accu | (long) *sp++); Next;
    Instruct(XORINT):
      accu = (value)(((long) accu ^ (long) *sp++) | 1); Next;
    Instruct(LSLINT):
      accu = (value)((((long) accu - 1) << Long_val(*sp++)) + 1); Next;
    Instruct(LSRINT):
      accu = (value)((((unsigned long) accu - 1) >> Long_val(*sp++)) | 1);
      Next;
    Instruct(ASRINT):
      accu = (value)((((long) accu - 1) >> Long_val(*sp++)) | 1); Next;

#define Integer_comparison(opname,tst) \
    Instruct(opname): \
      accu = Val_int((long) accu tst (long) *sp++); Next;

    Integer_comparison(EQ, ==)
    Integer_comparison(NEQ, !=)
    Integer_comparison(LTINT, <)
    Integer_comparison(LEINT, <=)
    Integer_comparison(GTINT, >)
    Integer_comparison(GEINT, >=)

    Instruct(OFFSETINT):
      accu += *pc << 1;
      pc++;
      Next;
    Instruct(OFFSETREF):
      Field(accu, 0) += *pc << 1;
      accu = Val_unit;
      pc++;
      Next;

/* Object-oriented operations */

#define Lookup(obj, lab) \
  Field (Field (Field (obj, 0), ((lab) >> 16) / sizeof (value)), \
	 ((lab) / sizeof (value)) & 0xFF)

    Instruct(GETMETHOD):
      accu = Lookup(sp[0], accu);
      Next;

/* Machine control */

    Instruct(STOP):
      external_raise = initial_external_raise;
      extern_sp = sp;
      return accu;

#ifndef THREADED_CODE
    default:
      fatal_error("bad opcode");
    }
  }
#endif
}

static opcode_t callback1_code[] = { ACC1, APPLY1, POP, 1, STOP };
static opcode_t callback2_code[] = { ACC2, APPLY2, POP, 1, STOP };
static opcode_t callback3_code[] = { ACC3, APPLY3, POP, 1, STOP };

value callback(closure, arg)
     value closure, arg;
{
  value res;
  extern_sp -= 2;
  extern_sp[0] = arg;
  extern_sp[1] = closure;
  callback_depth++;
  res = interprete(callback1_code, sizeof(callback1_code));
  callback_depth--;
  return res;
}

value callback2(closure, arg1, arg2)
     value closure, arg1, arg2;
{
  value res;
  extern_sp -= 3;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = closure;
  callback_depth++;
  res = interprete(callback2_code, sizeof(callback2_code));
  callback_depth--;
  return res;
}

value callback3(closure, arg1, arg2, arg3)
     value closure, arg1, arg2, arg3;
{
  value res;
  extern_sp -= 4;
  extern_sp[0] = arg1;
  extern_sp[1] = arg2;
  extern_sp[2] = arg3;
  extern_sp[3] = closure;
  callback_depth++;
  res = interprete(callback3_code, sizeof(callback3_code));
  callback_depth--;
  return res;
}
