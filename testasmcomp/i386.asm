        .globl  _call_gen_code
        .align  4
_call_gen_code:
        pushl %ebp
        movl %esp,%ebp
        pushl %ebx
        pushl %esi
        pushl %edi
        movl 12(%ebp),%eax
        movl 16(%ebp),%ebx
        movl 20(%ebp),%ecx
        movl 24(%ebp),%edx
        fldz
        fldz
        fldz
        fldz
        call *8(%ebp)
        popl %edi
        popl %esi
        popl %ebx
        popl %ebp
        ret

        .globl  _caml_c_call
        .align  4
_caml_c_call:
        jmp     *%eax

        .comm   _caml_exception_pointer, 4
        .comm   _young_ptr, 4
        .comm   _young_start, 4
