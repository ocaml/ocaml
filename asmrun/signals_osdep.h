/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2004 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Processor- and OS-dependent signal interface */

/****************** Alpha, all OS */

#if defined(TARGET_alpha)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, int code, struct sigcontext * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = 0

  typedef long context_reg;
  #define CONTEXT_PC (context->sc_pc)
  #define CONTEXT_EXCEPTION_POINTER (context->sc_regs[15])
  #define CONTEXT_YOUNG_LIMIT (context->sc_regs[13])
  #define CONTEXT_YOUNG_PTR (context->sc_regs[14])

/****************** AMD64, Linux */

#elif defined(TARGET_amd64) && defined (SYS_linux)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, siginfo_t * info, ucontext_t * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_sigaction = (void (*)(int,siginfo_t *,void *)) (name); \
     sigact.sa_flags = SA_SIGINFO

  typedef greg_t context_reg;
  #define CONTEXT_PC (context->uc_mcontext.gregs[REG_RIP])
  #define CONTEXT_EXCEPTION_POINTER (context->uc_mcontext.gregs[REG_R14])
  #define CONTEXT_YOUNG_PTR (context->uc_mcontext.gregs[REG_R15])
  #define CONTEXT_FAULTING_ADDRESS ((char *) context->uc_mcontext.gregs[REG_CR2])

/****************** I386, Linux */

#elif defined(TARGET_i386) && defined(SYS_linux_elf)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, struct sigcontext context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = 0

  #define CONTEXT_FAULTING_ADDRESS ((char *) context.cr2)

/****************** I386, BSD */

#elif defined(TARGET_i386) && defined(SYS_bsd)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, siginfo_t * info, void * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_sigaction = (name);
     sigact.sa_flags = SA_SIGINFO

  #define CONTEXT_FAULTING_ADDRESS ((char *) info->si_addr)

/****************** MIPS, all OS */

#elif defined(TARGET_mips)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, int code, struct sigcontext * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = 0

  typedef int context_reg;
  #define CONTEXT_PC (context->sc_pc)
  #define CONTEXT_EXCEPTION_POINTER (context->sc_regs[30])
  #define CONTEXT_YOUNG_LIMIT (context->sc_regs[22])
  #define CONTEXT_YOUNG_PTR (context->sc_regs[23])

/****************** PowerPC, MacOS X */

#elif defined(TARGET_power) && defined(SYS_rhapsody)

  #include <sys/utsname.h>

  #define DECLARE_SIGNAL_HANDLER(name) \
     static void name(int sig, int code, void * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = SA_SIGINFO

  typedef unsigned long context_reg;
  #define CONTEXT_PC (*context_gpr_p(context, -2))
  #define CONTEXT_EXCEPTION_POINTER (*context_gpr_p(context, 29))
  #define CONTEXT_YOUNG_LIMIT (*context_gpr_p(context, 30))
  #define CONTEXT_YOUNG_PTR (*context_gpr_p(context, 31))

  static int ctx_version = 0;
  static void init_ctx (void)
  {
    struct utsname name;
    if (uname (&name) == 0){
      if (name.release[1] == '.' && name.release[0] <= '5'){
        ctx_version = 1;
      }else{
        ctx_version = 2;
      }
    }else{
      caml_fatal_error ("cannot determine SIGCONTEXT format");
    }
  }

  #ifdef DARWIN_VERSION_6
    #include <sys/ucontext.h>
    static unsigned long *context_gpr_p (void *ctx, int regno)
    {
      unsigned long *regs;
      if (ctx_version == 0) init_ctx ();
      if (ctx_version == 1){
        /* old-style context (10.0 and 10.1) */
        regs = (unsigned long *)(((struct sigcontext *)ctx)->sc_regs);
      }else{
        Assert (ctx_version == 2);
        /* new-style context (10.2) */
        regs = (unsigned long *)&(((struct ucontext *)ctx)->uc_mcontext->ss);
      }
      return &(regs[2 + regno]);
    }
  #else
    #define SA_SIGINFO 0x0040
    struct ucontext {
      int       uc_onstack;
      sigset_t  uc_sigmask;
      struct sigaltstack uc_stack;
      struct ucontext   *uc_link;
      size_t    uc_mcsize;
      unsigned long     *uc_mcontext;
    };
    static unsigned long *context_gpr_p (void *ctx, int regno)
    {
      unsigned long *regs;
      if (ctx_version == 0) init_ctx ();
      if (ctx_version == 1){
        /* old-style context (10.0 and 10.1) */
        regs = (unsigned long *)(((struct sigcontext *)ctx)->sc_regs);
      }else{
        Assert (ctx_version == 2);
        /* new-style context (10.2) */
        regs = (unsigned long *)((struct ucontext *)ctx)->uc_mcontext + 8;
      }
      return &(regs[2 + regno]);
    }
  #endif

/****************** PowerPC, ELF (Linux) */

#elif defined(TARGET_power) && defined(SYS_elf)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, struct sigcontext * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = 0

  typedef unsigned long context_reg;
  #define CONTEXT_PC (context->regs->nip)
  #define CONTEXT_EXCEPTION_POINTER (context->regs->gpr[29])
  #define CONTEXT_YOUNG_LIMIT (context->regs->gpr[30])
  #define CONTEXT_YOUNG_PTR (context->regs->gpr[31])

/****************** PowerPC, BSD */

#elif defined(TARGET_power) && defined(SYS_bsd)

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, int code, struct sigcontext * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (void (*)(int)) (name); \
     sigact.sa_flags = 0

  typedef unsigned long context_reg;
  #define CONTEXT_EXCEPTION_POINTER (context->sc_frame.fixreg[29])
  #define CONTEXT_YOUNG_LIMIT (context->sc_frame.fixreg[30])
  #define CONTEXT_YOUNG_PTR (context->sc_frame.fixreg[31])

/****************** SPARC, Solaris */

#elif defined(TARGET_sparc) && defined(SYS_solaris)

  #include <ucontext.h>

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig, siginfo_t * info, ucontext_t * context)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_sigaction = (void (*)(int,siginfo_t *,void *)) (name); \
     sigact.sa_flags = SA_SIGINFO

  typedef long context_reg;
  #define CONTEXT_PC (context->uc_mcontext.gregs[REG_PC])
    /* Local register number N is saved on the stack N words
       after the stack pointer */
  #define SPARC_L_REG(n) ((long *)(context->uc_mcontext.gregs[REG_SP]))[n]
  #define CONTEXT_EXCEPTION_POINTER (SPARC_L_REG(5))
  #define CONTEXT_YOUNG_LIMIT (SPARC_L_REG(7))
  #define CONTEXT_YOUNG_PTR (SPARC_L_REG(6))

/******************** Default */

#else

  #define DECLARE_SIGNAL_HANDLER(name) \
    static void name(int sig)

  #define SET_SIGACT(sigact,name) \
     sigact.sa_handler = (name); \
     sigact.sa_flags = 0

#endif
