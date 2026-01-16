;**************************************************************************
;*                                                                        *
;*                                 OCaml                                  *
;*                                                                        *
;*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *
;*                                                                        *
;*   Copyright 2006 Institut National de Recherche en Informatique et     *
;*     en Automatique.                                                    *
;*                                                                        *
;*   All rights reserved.  This file is distributed under the terms of    *
;*   the GNU Lesser General Public License version 2.1, with the          *
;*   special exception on linking described in the file LICENSE.          *
;*                                                                        *
;**************************************************************************

; Asm part of the runtime system, AMD64 processor, Intel syntax

; Notes on Win64 calling conventions:
;     function arguments in RCX, RDX, R8, R9 / XMM0 - XMM3
;     caller must reserve 32 bytes of stack space
;     callee must preserve RBX, RBP, RSI, RDI, R12-R15, XMM6-XMM15

        EXTRN  caml_garbage_collection: NEAR
        EXTRN  caml_apply2: NEAR
        EXTRN  caml_apply3: NEAR
        EXTRN  caml_program: NEAR
        EXTRN  caml_array_bound_error_asm: NEAR
        EXTRN  caml_stash_backtrace: NEAR
        EXTRN  caml_try_realloc_stack: NEAR
        EXTRN  caml_try_realloc_stack: NEAR
        EXTRN  caml_exn_Stack_overflow: NEAR
        EXTRN  caml_raise_unhandled_effect: NEAR
        EXTRN  caml_raise_continuation_already_resumed: NEAR
        EXTRN  caml_free_stack: NEAR

; Load caml/domain_state.tbl (via domain_state.inc, to remove C-style comments)
        domain_curr_field = 0
DOMAIN_STATE MACRO _type:REQ, name:REQ
        domain_field_caml_&name EQU domain_curr_field
        domain_curr_field = domain_curr_field + 1
        ; Returning a value turns DOMAIN_STATE into a macro function, which
        ; causes the bracketed parameters to be both required and correctly
        ; parsed. Returning an empty string allows this to be used as though
        ; it were a macro procedure.
        EXITM <>
ENDM

INCLUDE domain_state.inc

; Caml_state(field) expands to the address of field in Caml_state, which is
; always stored in r14.
Caml_state MACRO field:REQ
        EXITM @CatStr(<[r14+>, %(domain_field_caml_&field), <*8]>)
ENDM

SAVE_ALL_REGS MACRO
    ; Save young_ptr
        mov     Caml_state(young_ptr), r15
    ; Now, use r15 to point to the gc_regs bucket
    ; We save r11 first to allow it to be scratch
        mov     r15, Caml_state(gc_regs_buckets)
        mov    qword ptr [r15 + 11*8], r11
        mov     r11,qword ptr [r15] ; next ptr
        mov     Caml_state(gc_regs_buckets), r11
        mov    qword ptr [r15 + 0*8], rax
        mov    qword ptr [r15 + 1*8], rbx
        mov    qword ptr [r15 + 2*8], rdi
        mov    qword ptr [r15 + 3*8], rsi
        mov    qword ptr [r15 + 4*8], rdx
        mov    qword ptr [r15 + 5*8], rcx
        mov    qword ptr [r15 + 6*8], r8
        mov    qword ptr [r15 + 7*8], r9
        mov    qword ptr [r15 + 8*8], r12
        mov    qword ptr [r15 + 9*8], r13
        mov    qword ptr [r15 + 10*8], r10
              ;qword ptr [r15 + 11*8] contains r11 already
        mov    qword ptr [r15 + 12*8], rbp
        movsd   mmword ptr [r15 + (0+13)*8], xmm0
        movsd   mmword ptr [r15 + (1+13)*8], xmm1
        movsd   mmword ptr [r15 + (2+13)*8], xmm2
        movsd   mmword ptr [r15 + (3+13)*8], xmm3
        movsd   mmword ptr [r15 + (4+13)*8], xmm4
        movsd   mmword ptr [r15 + (5+13)*8], xmm5
        movsd   mmword ptr [r15 + (6+13)*8], xmm6
        movsd   mmword ptr [r15 + (7+13)*8], xmm7
        movsd   mmword ptr [r15 + (8+13)*8], xmm8
        movsd   mmword ptr [r15 + (9+13)*8], xmm9
        movsd   mmword ptr [r15 + (10+13)*8], xmm10
        movsd   mmword ptr [r15 + (11+13)*8], xmm11
        movsd   mmword ptr [r15 + (12+13)*8], xmm12
        movsd   mmword ptr [r15 + (13+13)*8], xmm13
        movsd   mmword ptr [r15 + (14+13)*8], xmm14
        movsd   mmword ptr [r15 + (15+13)*8], xmm15
ENDM

RESTORE_ALL_REGS MACRO
    ; Restore rax, freeing up the next ptr slot
        mov     rax,qword ptr [r15 + 0*8]
        mov     r11, Caml_state(gc_regs_buckets)
        mov     qword ptr [r15], r11 ; next ptr
        mov     Caml_state(gc_regs_buckets), r15
    ; above:    rax,qword ptr [r15 + 0*8]
        mov     rbx,qword ptr [r15 + 1*8]
        mov     rdi,qword ptr [r15 + 2*8]
        mov     rsi,qword ptr [r15 + 3*8]
        mov     rdx,qword ptr [r15 + 4*8]
        mov     rcx,qword ptr [r15 + 5*8]
        mov     r8,qword ptr [r15 + 6*8]
        mov     r9,qword ptr [r15 + 7*8]
        mov     r12,qword ptr [r15 + 8*8]
        mov     r13,qword ptr [r15 + 9*8]
        mov     r10,qword ptr [r15 + 10*8]
        mov     r11,qword ptr [r15 + 11*8]
        mov     rbp,qword ptr [r15 + 12*8]
        movsd   xmm0, mmword ptr [r15 + (0+13)*8]
        movsd   xmm1, mmword ptr [r15 + (1+13)*8]
        movsd   xmm2, mmword ptr [r15 + (2+13)*8]
        movsd   xmm3, mmword ptr [r15 + (3+13)*8]
        movsd   xmm4, mmword ptr [r15 + (4+13)*8]
        movsd   xmm5, mmword ptr [r15 + (5+13)*8]
        movsd   xmm6, mmword ptr [r15 + (6+13)*8]
        movsd   xmm7, mmword ptr [r15 + (7+13)*8]
        movsd   xmm8, mmword ptr [r15 + (8+13)*8]
        movsd   xmm9, mmword ptr [r15 + (9+13)*8]
        movsd   xmm10, mmword ptr [r15 + (10+13)*8]
        movsd   xmm11, mmword ptr [r15 + (11+13)*8]
        movsd   xmm12, mmword ptr [r15 + (12+13)*8]
        movsd   xmm13, mmword ptr [r15 + (13+13)*8]
        movsd   xmm14, mmword ptr [r15 + (14+13)*8]
        movsd   xmm15, mmword ptr [r15 + (15+13)*8]
        mov     r15, Caml_state(young_ptr)
ENDM

SWITCH_OCAML_TO_C MACRO
    ; Fill in Caml_state->current_stack->sp
        mov     r10, Caml_state(current_stack)
        mov    qword ptr [r10], rsp
    ; Fill in Caml_state->c_stack
        mov     r11, Caml_state(c_stack)
        mov    qword ptr [r11 + 40], rsp
        mov    qword ptr [r11 + 32], r10
    ; Switch to C stack
        mov     rsp, qword ptr r11
ENDM

SWITCH_C_TO_OCAML MACRO
        mov     rsp,qword ptr [rsp+40]
ENDM

; Callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15

PUSH_CALLEE_SAVE_REGS MACRO
        push    rbx
        push    rbp
        push    rsi
        push    rdi
        push    r12
        push    r13
        push    r14
        push    r15
        sub     rsp, 10*16       ; stack 16-aligned + 10 saved xmm regs
        movupd  xmmword ptr [rsp + 0*16], xmm6
        movupd  xmmword ptr [rsp + 1*16], xmm7
        movupd  xmmword ptr [rsp + 2*16], xmm8
        movupd  xmmword ptr [rsp + 3*16], xmm9
        movupd  xmmword ptr [rsp + 4*16], xmm10
        movupd  xmmword ptr [rsp + 5*16], xmm11
        movupd  xmmword ptr [rsp + 6*16], xmm12
        movupd  xmmword ptr [rsp + 7*16], xmm13
        movupd  xmmword ptr [rsp + 8*16], xmm14
        movupd  xmmword ptr [rsp + 9*16], xmm15
ENDM

POP_CALLEE_SAVE_REGS MACRO
        movupd  xmm6, xmmword ptr [rsp + 0*16]
        movupd  xmm7, xmmword ptr [rsp + 1*16]
        movupd  xmm8, xmmword ptr [rsp + 2*16]
        movupd  xmm9, xmmword ptr [rsp + 3*16]
        movupd  xmm10, xmmword ptr [rsp + 4*16]
        movupd  xmm11, xmmword ptr [rsp + 5*16]
        movupd  xmm12, xmmword ptr [rsp + 6*16]
        movupd  xmm13, xmmword ptr [rsp + 7*16]
        movupd  xmm14, xmmword ptr [rsp + 8*16]
        movupd  xmm15, xmmword ptr [rsp + 9*16]
        add     rsp, 10*16
        pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rdi
        pop     rsi
        pop     rbp
        pop     rbx
ENDM

RESTORE_EXN_HANDLER_OCAML MACRO
        mov     rsp, Caml_state(exn_handler)
        lea     r11, Caml_state(exn_handler)
        pop     qword ptr [r11]
ENDM

SWITCH_OCAML_STACKS MACRO
        mov     qword ptr [rsi], rsp
        mov     r12, Caml_state(exn_handler)
        mov     qword ptr [rsi+8], r12
        mov     Caml_state(current_stack), r10
        mov     rsp,qword ptr [r10]
        mov     r12,qword ptr [r10+8]
        mov     Caml_state(exn_handler), r12
ENDM

        .CODE

; Allocation functions and GC interface.
; Referenced from C code in runtime/startup_nat.c
        PUBLIC  caml_system__code_begin
caml_system__code_begin:
        ret  ; just one instruction, so that debuggers don't display
             ; caml_system__code_begin instead of caml_call_gc

; Allocation

        PUBLIC  caml_call_realloc_stack
        ALIGN   4
caml_call_realloc_stack:
        SAVE_ALL_REGS
        mov     rcx,qword ptr [rsp+8]
        SWITCH_OCAML_TO_C
        call    caml_try_realloc_stack
        SWITCH_C_TO_OCAML
        cmp     rax, 0
        jz      L104
        RESTORE_ALL_REGS
        ret
L104:
        RESTORE_ALL_REGS
        lea     rax, caml_exn_Stack_overflow
        add     rsp, 16
        jmp     caml_raise_exn

        PUBLIC  caml_call_gc
        ALIGN   4
caml_call_gc:
        SAVE_ALL_REGS
        mov     Caml_state(gc_regs), r15
    ; Call the garbage collector
        SWITCH_OCAML_TO_C
        call    caml_garbage_collection
        SWITCH_C_TO_OCAML
        mov     r15, Caml_state(gc_regs)
        RESTORE_ALL_REGS
    ; Return to caller
        ret

        PUBLIC  caml_alloc1
        ALIGN   4
caml_alloc1:
        sub     r15, 16
        cmp     r15, Caml_state(young_limit)
        jb      caml_call_gc
        ret

        PUBLIC  caml_alloc2
        ALIGN   4
caml_alloc2:
        sub     r15, 24
        cmp     r15, Caml_state(young_limit)
        jb      caml_call_gc
        ret

        PUBLIC  caml_alloc3
        ALIGN   4
caml_alloc3:
        sub     r15, 32
        cmp     r15, Caml_state(young_limit)
        jb      caml_call_gc
        ret

        PUBLIC  caml_allocN
        ALIGN   4
caml_allocN:
        cmp     r15, Caml_state(young_limit)
        jb      caml_call_gc
        ret

; Call a C function from OCaml

; Update [young_limit] when returning from non-noalloc extern calls.
; Here is C code that can be used to generate RET_FROM_C_CALL for a
; new back-end.

;   #include <stdatomic.h>
;   #include <stdint.h>

;   typedef struct { _Atomic(uint64_t) young_limit;
;                    _Bool action_pending; } caml_domain_state;

;   void ret_from_c_call(caml_domain_state *dom_st)
;   {
;     if (__builtin_expect(dom_st->action_pending, 0))
;       atomic_store_explicit(&dom_st->young_limit, (uint64_t)-1,
;                             memory_order_relaxed);
;   }

RET_FROM_C_CALL MACRO
        LOCAL L1
        cmp     byte ptr Caml_state(action_pending), 0
        jne     L1
        ret
L1:
        mov     qword ptr Caml_state(young_limit), -1
        ret
ENDM

        PUBLIC  caml_c_call
        ALIGN   4
caml_c_call:
    ; Arguments:
    ;  C arguments         : rcx, rdx, r8 and r9
    ;  C function          : rax
        SWITCH_OCAML_TO_C
    ; Make the alloc ptr available to the C code
        mov     Caml_state(young_ptr), r15
    ; Call the function (address in rax)
        call    rax
    ; Prepare for return to OCaml
        mov     r15, Caml_state(young_ptr)
    ; Load OCaml stack and restore global variables
        SWITCH_C_TO_OCAML
    ; Return to OCaml caller
        RET_FROM_C_CALL

        PUBLIC  caml_c_call_stack_args
        ALIGN 4
caml_c_call_stack_args:
    ; Arguments:
    ;  C arguments         : rcx, rdx, r8 and r9
    ;    C function          : rax
    ;    C stack args        : begin=r13 end=r12
    ; Switch from OCaml to C
        SWITCH_OCAML_TO_C
    ; Make the alloc ptr available to the C code
        mov     Caml_state(young_ptr), r15
    ; Copy the arguments and call
        call    caml_c_call_copy_stack_args
    ; Prepare for return to OCaml
        mov     r15, Caml_state(young_ptr)
    ; Load ocaml stack and restore global variables
        SWITCH_C_TO_OCAML
    ; Return to OCaml caller
        RET_FROM_C_CALL

; To correctly maintain frame pointers during stack reallocation,
; the runtime assumes that the caml_c_call stub does not push
; anything to the stack before the first frame pointer on the C stack.
; To guarantee this when stack arguments are used, the actual pushing
; of arguments is done by this separate function
        PUBLIC caml_c_call_copy_stack_args
        ALIGN 4
caml_c_call_copy_stack_args:
    ; Set up a frame pointer even without WITH_FRAME_POINTERS,
    ; which we use to pop an unknown number of arguments later
        push    rbp
        mov     rbp, rsp
    ; Copy arguments from OCaml to C stack
L105:
        sub     r12, 8
        cmp     r12,r13
        jb      L210
        push    qword ptr [r12]
        jmp     L105
L210:
    ; Allocate the shadow store on Windows (the c_stack_link store was used
    ; in calling caml_c_call_copy_stack_args)
        sub     rsp, 32
    ; Call the function (address in %rax)
        call    rax
    ; Pop arguments back off the stack
        mov     rsp, rbp
        pop     rbp
        ret

; Start the OCaml program

        PUBLIC  caml_start_program
        ALIGN   4
caml_start_program:
    ; Save callee-save registers
        PUSH_CALLEE_SAVE_REGS
    ; First argument (rcx) is Caml_state. Load it in r14
        mov     r14, rcx
    ; Initial entry point is caml_program
        lea     r12, caml_program
    ; Common code for caml_start_program and caml_callback*
L106:
    ; Load young_ptr into %r15
        mov     r15, Caml_state(young_ptr)
    ; Build struct c_stack_link on the C stack
        sub     rsp, 56 ; sizeof struct c_stack_link
        mov     qword ptr [rsp + 32], 0
        mov     qword ptr [rsp + 40], 0
        mov     r10, Caml_state(c_stack)
        mov     qword ptr [rsp + 48], r10
        mov     Caml_state(c_stack), rsp
    ; Load the OCaml stack.
        mov     r11, Caml_state(current_stack)
        mov     r10, qword ptr [r11]
    ; Store the stack pointer to allow DWARF unwind XXX
        sub     r10, 16
        mov     qword ptr [r10], rsp ; C_STACK_SP
    ; Store the gc_regs for callbacks during a GC
        mov     r11, Caml_state(gc_regs)
        mov     qword ptr [r10 + 8], r11
    ; Build a handler for exceptions raised in OCaml on the OCaml stack.
        sub     r10, 16
        lea     r11, L108
        mov     qword ptr [r10 + 8], r11
    ; link in the previous exn_handler so that copying stacks works
        mov     r11, Caml_state(exn_handler)
        mov     qword ptr [r10], r11
        mov     Caml_state(exn_handler), r10
    ; Switch stacks and call the OCaml code
        mov     rsp, r10
        call    r12
L107:
    ; Pop the exception handler
        mov     r11, qword ptr [rsp]
        mov     Caml_state(exn_handler), r11
        lea     r10, [rsp+16]
L109:
    ; Restore GC regs
        mov     r11, qword ptr [r10+8]
        mov     Caml_state(gc_regs), r11
        add     r10, 16
    ; Update alloc ptr
        mov     Caml_state(young_ptr), r15
    ; Return to C stack.
        mov     r11, Caml_state(current_stack)
        mov     qword ptr [r11], r10
        mov     rsp, Caml_state(c_stack)
    ; Pop the struct c_stack_link
        mov     r10, qword ptr [rsp+48]
        mov     Caml_state(c_stack), r10
        add     rsp, 56
    ; Restore callee-save registers.
        POP_CALLEE_SAVE_REGS
    ; Return to caller
        ret
L108:
    ; Exception handler
    ; Mark the bucket as an exception result and return it
        or      rax, 2
        ; exn handler already popped here
        mov     r10, rsp
        jmp     L109

; Raise an exception from OCaml

        PUBLIC  caml_raise_exn
        ALIGN   4
caml_raise_exn:
        test    qword ptr Caml_state(backtrace_active), 1
        jne     L110
        RESTORE_EXN_HANDLER_OCAML
        ret
L110:
        mov     qword ptr Caml_state(backtrace_pos), 0
L117:
        mov     r13, rsp             ; Save OCaml stack pointer
        mov     r12, rax             ; Save exception bucket
        mov     rsp, Caml_state(c_stack)
        mov     rcx, rax             ; Arg 1: exception bucket
        mov     rdx, qword ptr [r13] ; Arg 2: PC of raise
        lea     r8, [r13+8]          ; Arg 3: SP of raise
        mov     r9, Caml_state(exn_handler) ; Arg 4: SP of handler
        call    caml_stash_backtrace
        mov     rax, r12             ; Recover exception bucket
        RESTORE_EXN_HANDLER_OCAML
        ret

        PUBLIC  caml_reraise_exn
        ALIGN   4
caml_reraise_exn:
        test     qword ptr Caml_state(backtrace_active), 1
        jne     L117
        RESTORE_EXN_HANDLER_OCAML
        ret

; Raise an exception from C

        PUBLIC  caml_raise_exception
        ALIGN   4
caml_raise_exception:
        mov     r14, rcx                   ; First argument is Caml_state
        mov     rax, rdx                   ; Second argument is exn bucket
        mov     r15, Caml_state(young_ptr) ; Reload alloc ptr
    ; Discard the C stack pointer and reset to OCaml stack
        mov     r10, Caml_state(current_stack)
        mov     rsp, qword ptr [r10]
        jmp     caml_raise_exn

; Callback from C to OCaml

        PUBLIC  caml_callback_asm
        ALIGN   4
caml_callback_asm:
        PUSH_CALLEE_SAVE_REGS
    ; Initial loading of arguments
        mov     r14, rcx      ; Caml_state
        mov     rbx, rdx      ; closure
        mov     rax, qword ptr [r8]     ; argument
        mov     r12, qword ptr [rbx]    ; code pointer
        mov     rdi, 0 ; XXX dummy?
        mov     rsi, 0 ; XXX dummy?
        jmp     L106

        PUBLIC  caml_callback2_asm
        ALIGN   4
caml_callback2_asm:
        PUSH_CALLEE_SAVE_REGS
    ; Initial loading of arguments
        mov     r14, rcx        ; Caml_state
        mov     rdi, rdx        ; closure
        mov     rax, qword ptr [r8]       ; first argument
        mov     rbx, qword ptr [r8 + 8]   ; second argument
        lea     r12, caml_apply2  ; code pointer
        mov     rsi, 0            ; XXX dummy?
        jmp     L106

        PUBLIC  caml_callback3_asm
        ALIGN   4
caml_callback3_asm:
        PUSH_CALLEE_SAVE_REGS
    ; Initial loading of arguments
        mov     r14, rcx        ; Caml_state
        mov     rax, qword ptr [r8]       ; first argument
        mov     rbx, qword ptr [r8 + 8]   ; second argument
        mov     rsi, rdx        ; closure
        mov     rdi, qword ptr [r8 + 16]  ; third argument
        lea     r12, caml_apply3      ; code pointer
        jmp     L106

; Fibers

        PUBLIC  caml_perform
        ALIGN   4
caml_perform:
    ;  %rax: effect to perform
    ;  %rbx: freshly allocated continuation
        mov     rsi, Caml_state(current_stack) ; %rsi := old stack
        lea     rdi, [rsi + 1] ; %rdi (last_fiber) := Val_ptr(old stack)
        mov     qword ptr [rbx], rdi ; Initialise continuation
do_perform:
    ;  %rax: effect to perform
    ;  %rbx: continuation
    ;  %rdi: last_fiber
    ;  %rsi: old stack *;
        mov     qword ptr [rbx+8], rdi  ; Set last fiber field in continuation
        mov     r11, qword ptr [rsi+16] ; %r11 := old stack -> handler
        mov     r10, qword ptr [r11+24] ; %r10 := parent stack
        cmp     r10, 0                  ; parent is NULL?
        je      L112
        SWITCH_OCAML_STACKS ; preserves r11 and rsi
     ; We have to null the Handler_parent after the switch because the
     ; Handler_parent is needed to unwind the stack for backtraces
        mov     qword ptr [r11+24], 0 ; Set parent of performer to NULL
        mov     rsi, qword ptr [r11+16]  ; %rsi := effect handler
        jmp     caml_apply3
L112:
    ; Switch back to original performer before raising Effect.Unhandled
    ; (no-op unless this is a reperform)
        mov     r10, qword ptr [rbx]  ; load performer stack from continuation
        sub     r10, 1       ; r10 := Ptr_val(r10)
        mov     rsi, Caml_state(current_stack)
        SWITCH_OCAML_STACKS
    ; No parent stack. Raise Effect.Unhandled.
        mov     rcx, rax
        lea     rax, caml_raise_unhandled_effect
        jmp     caml_c_call

        PUBLIC  caml_reperform
        ALIGN   4
caml_reperform:
    ;  %rax: effect to reperform
    ;  %rbx: continuation
    ;  %rdi: last_fiber
        mov     rsi, Caml_state(current_stack)    ; %rsi := old stack
        mov     r10, qword ptr [rdi+15]
        mov     qword ptr [r10+24], rsi       ; Append to last_fiber
        lea     rdi, [rsi + 1]  ; %rdi (last_fiber) := Val_ptr(old stack)
        jmp     do_perform

        PUBLIC  caml_resume
        ALIGN   4
caml_resume:
    ; %rax -> fiber, %rbx -> fun, %rdi -> arg, %rsi -> last_fiber
        lea     r10, [rax-1]  ; %r10 (new stack) = Ptr_val(%rax)
        mov     rax, rdi      ; %rax := argument to the function in %rbx
    ;  check if stack null, then already used
        test    r10, r10
        jz      L502
    ; Add current stack to the last fiber
        mov     rdi, qword ptr [rsi+15]
        mov     rsi, Caml_state(current_stack)
        mov     qword ptr [rdi+24], rsi
        SWITCH_OCAML_STACKS
        jmp     qword ptr [rbx]
L502:
        lea     rax, caml_raise_continuation_already_resumed
        jmp     caml_c_call

        PUBLIC  caml_runstack
        ALIGN   4
caml_runstack:
    ; %rax -> fiber, %rbx -> fun, %rdi -> arg
        and     rax, -2       ; %rax = Ptr_val(%rax)
    ; save old stack pointer and exception handler
        mov     rcx, Caml_state(current_stack)
        mov     r10, Caml_state(exn_handler)
        mov     qword ptr [rcx], rsp
        mov     qword ptr [rcx+8], r10
    ; Load new stack pointer and set parent
        mov     r11, qword ptr [rax+16]
        mov     qword ptr [r11+24], rcx
        mov     Caml_state(current_stack), rax
        mov     r11, qword ptr [rax]
    ; Create an exception handler on the target stack
    ;  after 16byte DWARF & gc_regs block (which is unused here)
        sub     r11, 32
        lea     r10, fiber_exn_handler
        mov     qword ptr [r11+8], r10
    ; link the previous exn_handler so that copying stacks works
        mov     r10, qword ptr [rax+8]
        mov     qword ptr [r11], r10
        mov     Caml_state(exn_handler), r11
    ; Switch to the new stack
        mov     rsp, r11
        mov     rax, rdi ; first argument
        call    qword ptr [rbx] ; closure in %rbx (second argument)
frame_runstack:
        lea     r11, [rsp+32] ; SP with exn handler popped
        mov     rbx, qword ptr [r11]
L610:
        mov     rcx, Caml_state(current_stack) ; arg to caml_free_stack
    ; restore parent stack and exn_handler into Caml_state
        mov     r10, qword ptr [r11+24]
        mov     r11, qword ptr [r10+8]
        mov     Caml_state(current_stack), r10
        mov     Caml_state(exn_handler), r11
    ; free old stack by switching directly to c_stack; is a no-alloc call
        mov     r13, qword ptr [r10]    ; saved across C call
        mov     r12, rax ; save %rax across C call
        mov     rsp, Caml_state(c_stack)
        call  caml_free_stack
    ; switch directly to parent stack with correct return
        mov     rsp, r13
        mov     rax, r12
    ; Invoke handle_value (or handle_exn)
        jmp     qword ptr [rbx]
fiber_exn_handler:
        lea     r11, [rsp+16]
        mov     rbx, qword ptr [r11+8]
        jmp     L610

        PUBLIC  caml_ml_array_bound_error
        ALIGN   4
caml_ml_array_bound_error:
        lea     rax, caml_array_bound_error_asm
        jmp     caml_c_call

        PUBLIC  caml_assert_stack_invariants
        ALIGN   4
caml_assert_stack_invariants:
        mov     r11, Caml_state(current_stack)
        mov     r10, rsp
        sub     r10, r11
        cmp     r10, 296
        jge     L310
        int     3
L310:
        ret

        PUBLIC caml_system__code_end
caml_system__code_end:

; Frametable - GC roots for callback
; Uses the same naming convention as ocamlopt generated modules.
        .DATA
        PUBLIC  caml_system$frametable
caml_system$frametable LABEL QWORD
        QWORD   2           ; two descriptors
        QWORD   L107        ; return address into callback
        WORD    -1          ; negative frame size => use callback link
        WORD    0           ; no roots here
        ALIGN   8
        QWORD   frame_runstack
        WORD    -1
        WORD    0

        PUBLIC  caml_negf_mask
        ALIGN   16
caml_negf_mask LABEL QWORD
        QWORD   8000000000000000H, 0

        PUBLIC  caml_absf_mask
        ALIGN   16
caml_absf_mask LABEL QWORD
        QWORD   7FFFFFFFFFFFFFFFH, 0FFFFFFFFFFFFFFFFH

        END
