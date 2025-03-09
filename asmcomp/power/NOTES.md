# Supported platforms

IBM POWER processors, 64-bit, little-endian, ELF ABI v2.
(Called `ppc64el` in Debian.)

No longer supported:
* 32 bits, ELF ABI (Debian's `powerpc`)
* 64 bits big-endian, ELF ABI v1 (Debian's `powerpc`)
* AIX
* MacOS X.

# Reference documents

* Instruction set architecture:
  _Power ISA User Instruction Set Architecture_,
  book 1 of _Power Instruction Set_
  (https://openpowerfoundation.org/specifications/isa/)

* ELF ABI 64 bits version 2:
  _OpenPOWER ABI for Linux Supplement for the Power Architecture
   64-bit ELF V2 ABI_
  (https://openpowerfoundation.org/specifications/64bitelfabi/)

* _The PowerPC Compiler Writer's Guide_, Warthman Associates, 1996.
  (PDF available from various sources on the Web.)

# Notes on calling conventions

## The minimal stack frame

The stack pointer SP (register 1) is always 16-byte aligned.

Every stack frame contains a 32-byte reserved area at SP to SP + 31.

* The 64-bit word at SP + 0 is reserved for a back pointer to the
  previous frame.  ocamlopt-generated code does not maintain this back link.
* The 32-bit word at SP + 8 is reserved for saving the CR register.
  ocamlopt-generated code does not use this word.
* The 32-bit word at SP + 12 is reserved for future uses.
* The 64-bit word at SP + 16 is used by the *callee* to save its return address
  (the value of the LR register on entry to the callee).  So, a function
  saves its return address in the callee's frame, at offset 16 from the SP
  on entry to the function.
* The 64-bit word at SP + 24 is used by the *caller* or by
  linker-generated call *veeners* to save the TOC register r2 across calls.

A typical function prologue that allocates N bytes of stack and saves LR:
```
        addi 1, 1, -N
        mflr 0
        std  0, (N + 16)(1)    ; in caller's frame!
```
If the back pointer is to be maintained as well:
```
        stdu 1, -N(1)
        mflr 0
        std  0, (N + 16)(1)    ; in caller's frame!
```

If the function needs to stack-allocate M extra bytes, e.g. for an
exception handler, it should leave the bottom 32 bytes reserved.  So,
it decrements SP by M and uses bytes SP + 32 ... SP + 32 + M - 1 as
the stack-allocated space:
```
        addi 1, 1, -32          ; allocate exception handler
        std  29, 40(1)          ; save previous trap ptr at handler + 8
```

Because the bottom 32 bytes are used by linker-generated call veeners,
we need to reserve them both in C stack frames and in OCaml stack
frames.

To speed up stack switching between OCaml and C, the stack pointers
saved in struct stack_info and struct c_stack_link point to the bottom
of the stack frame and include the reserved 32 bytes area.

## TOC references and register r2

Register r2 points to a table of contents (TOC) that contains absolute
addresses for symbols.  Different compilation units can have different TOCs.
Hence, register r2 must be properly initialized at the beginning of
functions that access the TOC, and properly saved and restored around
function calls.

The general protocol is as follows:

* When a function is called, its address (the address of the first
  instruction of the function) must be in register r12.

* Every function that makes TOC references starts with two
  instructions that initialize r2 to a fixed offset from r12, offset
  determined by the linker:
```
0:  addis 2, 12, (.TOC. - 0b)@ha
    addi  2, 2,  (.TOC. - 0b)@l
```

* Register r2 is caller-save.  Hence, if the caller makes TOC
  references, it must save r2 before the call and restore it after.
  The reserved area of the current frame is typically used to save r2.

For example, here is a call to a function pointer found at offset 0
from register r3:
```
     std   2, 24(1)                 ; save r2
     ld    12, 0(3)                 ; address of function in r12
     mtctr 12
     bctrl                          ; call the function
     ld    2, 24(1)                 ; restore r2
```

If the caller does not make TOC references, the `std 2` and `ld 2` can
be omitted.  Likewise for a tail call:
```
     ld    12, 0(3)                 ; address of function in r12
     mtctr 12
     bctr                           ; tail call the function
```

For calls to known functions `bl function_name`, a special protocol is
implemented by the linker.  The object file contains the `bl`
instruction followed by a `nop` instruction:
```
     bl function_name
     nop
```
If the linker determines that `function_name` and the caller function
share the same TOC, it rewrites the code as
```
     bl function_name+8             ; skip the prologue that initializes r2
     nop
```
So, the callee does not recompute r2 from r12, it just uses the same
r2 value as the caller.  For this reason, r12 need not be initialized
by the caller.

If `function_name` uses a different TOC than the caller function, the
linker generates a branch-and-link to a veener function.  The veener
function saves r2, recomputes r12, and branches to `function_name`.
The `nop` after the branch-and-link becomes a reload of the caller's
TOC pointer in r2.
```
     bl veener_function
     ld 2, 24(1)             ; restore r2


veener_function:
     std 2, 24(1)            ; save caller's r2
     <load r12 with absolute address of function_name>
     mtctr r12
     bctr
```

However, this doesn't quite work for tail calls.  Consider:

- current function f1 calls f2 that has the same TOC;
- f2 tailcalls f3 that has a different TOC.

Because f1 and f2 have the same TOC, the linker inserted no code in f1
to save and restore r2 around the call to f2.

Because f2 tailcalls f3, r2 will not be restored to f2's TOC when f3 returns.

So, we're back into f1, with the wrong TOC in r2.

To avoid this problem, ocamlopt always reloads r2 after a direct call.
One possibility would be to save r2 and reload r2 around the direct
call, using the canonical slot in the current frame:
```
     std 2, 24(1)
     bl function_name
     nop
     ld 2, 24(1)
```
However, this adds 2 instructions to every direct call.  We can avoid
generating the store if the current function saves its r2 value
somewhere on the stack at the beginning.  Then, after each direct or
indirect call, we can reload r2 from there.

What is "there"?  Which location is used for saving r2 at the
beginning of the function?  A natural choice would be the word at SP + 24,
i.e. the TOC-saving word for the current function.  However, SP can
vary inside the function, e.g. to install and remove exception
handlers, so additional r2 saves would need to be generated.

The code currently generated by ocamlopt saves its r2 at SP + (N + 8)
where N is the size of the stack frame, that is, at the word reserved
by the callee for the caller to save the CR register, which
ocamlopt-generated code does not use otherwise.  This avoids
allocating one more word in the current frame just to save r2.
