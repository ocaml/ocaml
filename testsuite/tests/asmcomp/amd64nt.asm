        .CODE

        PUBLIC  call_gen_code
        ALIGN  16
call_gen_code:
        push    rbx
        push    rbp
        push    r12
        push    r13
        push    r14
        push    r15
        mov     rdi, r10
        mov     rsi, rax
        mov     rdx, rbx
        mov     rcx, rdi
        mov     r8, rsi
        call    r10
        pop     r15
        pop     r14
        pop     r13
        pop     r12
        pop     rbp
        pop     rbx
        ret

        PUBLIC  caml_c_call
        ALIGN  16
caml_c_call:
        jmp     rax

        PUBLIC caml_call_gc
        PUBLIC caml_alloc1
        PUBLIC caml_alloc2
        PUBLIC caml_alloc3
        PUBLIC caml_allocN
        PUBLIC caml_raise_exn
caml_call_gc:
caml_alloc1:
caml_alloc2:
caml_alloc3:
caml_allocN:
caml_raise_exn:
        int     3

        .DATA
        PUBLIC caml_exception_pointer
caml_exception_pointer QWORD 0, 0

        PUBLIC  caml_young_ptr
caml_young_ptr     LABEL QWORD
        QWORD  0, 0

        PUBLIC  caml_young_limit
caml_young_limit    LABEL QWORD
        QWORD  0, 0

        END
