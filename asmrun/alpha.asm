/* Asm part of the runtime system, Alpha processor */

        .comm   young_start 8
        .comm   young_end 8
        .comm   young_ptr 8
        .comm   gc_entry_regs 8 * 32
        .comm   gc_entry_float_regs 8 * 32
        .comm   caml_top_of_stack 8
        .comm   caml_bottom_of_stack 8
        .comm   caml_last_return_address 8
        .comm   caml_exception_pointer 8
        .comm   remembered_ptr 8
        .comm   remembered_end 8

#define SAVE(r) stq $/**/r, r * 8 ($24)
#define LOAD(r) ldq $/**/r, r * 8 ($24)
#define FSAVE(r) stt $f/**/r, r * 8 ($24)
#define FLOAD(r) ldt $f/**/r, r * 8 ($24)

#define SAVE_ALL_REGS \
    lda     $24, gc_entry_regs; \
    SAVE(0); SAVE(1); SAVE(2); SAVE(3); SAVE(4); SAVE(5); SAVE(6); SAVE(7); \
    SAVE(8); SAVE(9); SAVE(10); SAVE(11); SAVE(12); \
    SAVE(16); SAVE(17); SAVE(18); SAVE(19); SAVE(20); SAVE(21); \
    lda     $24, gc_entry_float_regs; \
    FSAVE(0); FSAVE(1); FSAVE(10); FSAVE(11); FSAVE(12); FSAVE(13); \
    FSAVE(14); FSAVE(15); FSAVE(16); FSAVE(17); FSAVE(18); FSAVE(19); \
    FSAVE(20); FSAVE(21); FSAVE(22); FSAVE(23); FSAVE(24); FSAVE(25); \
    FSAVE(26); FSAVE(27); FSAVE(28)

#define LOAD_ALL_REGS \
    lda     $24, gc_entry_regs; \
    LOAD(0); LOAD(1); LOAD(2); LOAD(3); LOAD(4); LOAD(5); LOAD(6); LOAD(7); \
    LOAD(8); LOAD(9); LOAD(10); LOAD(11); LOAD(12); \
    LOAD(16); LOAD(17); LOAD(18); LOAD(19); LOAD(20); LOAD(21); \
    lda     $24, gc_entry_float_regs; \
    FLOAD(0); FLOAD(1); FLOAD(10); FLOAD(11); FLOAD(12); FLOAD(13); \
    FLOAD(14); FLOAD(15); FLOAD(16); FLOAD(17); FLOAD(18); FLOAD(19); \
    FLOAD(20); FLOAD(21); FLOAD(22); FLOAD(23); FLOAD(24); FLOAD(25); \
    FLOAD(26); FLOAD(27); FLOAD(28)

/* Allocation */

        .text
        .globl  caml_alloc1
        .globl  caml_alloc2
        .globl  caml_alloc3
        .globl  caml_alloc
        .globl  caml_call_gc
        .ent    caml_alloc1

/* caml_alloc* : all code generator registers preserved,
   $gp preserved, $27 not valid on entry */

        .align  3
caml_alloc1:
        subq    $13, 16, $13
        cmpult  $13, $14, $25
        bne     $25, $100
        ret     ($26)
$100:   ldiq    $25, 16
        br      caml_call_gc

        .align  3
caml_alloc2:
        subq    $13, 24, $13
        cmpult  $13, $14, $25
        bne     $25, $101
        ret     ($26)
$101:   ldiq    $25, 24
        br      caml_call_gc

        .align  3
caml_alloc3:
        subq    $13, 32, $13
        cmpult  $13, $14, $25
        bne     $25, $102
        ret     ($26)
$102:   ldiq    $25, 32
        br      caml_call_gc

        .align  3
caml_alloc:
        subq    $13, $25, $13
        .set    noat
        cmpult  $13, $14, $at
        bne     $at, caml_call_gc
        .set    at
        ret     ($26)
        
caml_call_gc:
        lda     $sp, -16($sp)
        stq     $26, 0($sp)
        stq     $gp, 8($sp)
    /* Rebuild $gp */
        br      $26, $103
$103:   ldgp    $gp, 0($26)
    /* Record lowest stack address and return address */
        ldq     $24, 0($sp)
        stq     $24, caml_last_return_address
        lda     $24, 16($sp)
        stq     $24, caml_bottom_of_stack
    /* Save all regs used by the code generator in the arrays
    /* gc_entry_regs and gc_entry_float_regs. */
        SAVE_ALL_REGS
    /* Pass the desired size as first argument */
        mov     $25, $16
    /* Call the garbage collector */
        jsr     garbage_collection
    /* Restore all regs used by the code generator */
        ldgp    $gp, 0($26)
        LOAD_ALL_REGS
    /* Reload new allocation pointer and allocation limit */
        ldq     $13, young_ptr
        ldq     $14, young_start
    /* Return to caller */
        ldq     $26, 0($sp)
        ldq     $gp, 8($sp)
        lda     $sp, 16($sp)
        ret     ($26)

        .end    caml_alloc1

/* Modification */

        .globl  caml_modify
        .globl  caml_fast_modify
        .ent    caml_modify

        .align  3
caml_modify:
    /* Pointer to block in $25 */
        ldgp    $gp, 0($27)
        ldq     $24, -8($25)
        .set    noat
        and     $24, 1024, $at
        beq     $at, $104
        .set    at
        ret     ($26)

        .align  3
caml_fast_modify:
    /* Pointer to block in $25, header in $24 */
        ldgp    $gp, 0($27)
    /* Set "modified" bit in header */
$104:   or      $24, 1024, $24
        stq     $24, -8($25)
    /* Store address of object in remembered set */
        ldq     $24, remembered_ptr
        stq     $25, 0($24)
        addq    $24, 8, $25
        stq     $25, remembered_ptr
        ldq     $24, remembered_end
        cmplt   $25, $24, $25
        beq     $25, caml_modify_realloc
        ret     ($26)
        .set    at

    /* Reallocate the remembered set, while preserving all regs */
caml_modify_realloc:
        lda     $sp, -16($sp)
        stq     $26, 0($sp)
        SAVE_ALL_REGS
        jsr     realloc_remembered
        LOAD_ALL_REGS
        ldq     $26, 0($sp)
        lda     $sp, 16($sp)
        ret     ($26)

        .end    caml_modify

/* Call a C function from Caml */

        .globl  caml_c_call
        .ent    caml_c_call

        .align  3
caml_c_call:
    /* Function to call in $25 */
        ldgp    $gp, 0($27)
    /* Record lowest stack address and return address */
        stq     $26, caml_last_return_address
        stq     $sp, caml_bottom_of_stack
    /* Make the exception handler and alloc ptr available to the C code */
        stq     $13, young_ptr
        stq     $15, caml_exception_pointer
    /* Preserve return address */
        mov     $26, $13
    /* Call the function */
        mov     $25, $27
        jsr     ($25)
    /* Restore return address and alloc ptr */
        ldgp    $gp, 0($26)
        mov     $13, $26
        ldq     $13, young_ptr
        ret     ($26)

        .end    caml_c_call

/* Start the Caml program */

        .globl  caml_start_program
        .ent    caml_start_program
        .align  3
caml_start_program:
	lda	$sp, -128($sp)
        stq     $26, 0($sp)
    /* Save all callee-save registers */
        stq     $9, 8($sp)
        stq     $10, 16($sp)
        stq     $11, 24($sp)
        stq     $12, 32($sp)
        stq     $13, 40($sp)
        stq     $14, 48($sp)
        stq     $15, 56($sp)
        stt     $f2, 64($sp)
        stt     $f3, 72($sp)
        stt     $f4, 80($sp)
        stt     $f5, 88($sp)
        stt     $f6, 96($sp)
        stt     $f7, 104($sp)
        stt     $f8, 112($sp)
        stt     $f9, 120($sp)
    /* Build an exception handler */
	lda	$sp, -16($sp)
	lda	$0, stray_exn_handler
	stq	$0, 8($sp)
	mov	$sp, $15
        stq     $sp, caml_top_of_stack
    /* Initialize allocation registers */
	ldq	$13, young_ptr
	ldq	$14, young_start
    /* Go for it */
        jsr     caml_program
    /* Pop handler */
        lda     $sp, 16($sp)
    /* Return with zero code */
        clr     $0
    /* Restore registers */
stray_exn_handler:
        ldq     $26, 0($sp)
        ldq     $9, 8($sp)
        ldq     $10, 16($sp)
        ldq     $11, 24($sp)
        ldq     $12, 32($sp)
        ldq     $13, 40($sp)
        ldq     $14, 48($sp)
        ldq     $15, 56($sp)
        ldt     $f2, 64($sp)
        ldt     $f3, 72($sp)
        ldt     $f4, 80($sp)
        ldt     $f5, 88($sp)
        ldt     $f6, 96($sp)
        ldt     $f7, 104($sp)
        ldt     $f8, 112($sp)
        ldt     $f9, 120($sp)
        lda     $sp, 128($sp)
        ret     ($26)

        .end    caml_start_program

/* Raise an exception from C */

        .globl  raise_caml_exception
        .ent    raise_caml_exception
        .align  3
raise_caml_exception:
        ldgp    $gp, 0($27)
        mov     $16, $0
        ldq     $13, young_ptr
        ldq     $14, young_start
        ldq     $sp, caml_exception_pointer
        ldq     $15, 0($sp)
        ldq     $27, 8($sp)
        lda     $sp, 16($sp)
        jmp     ($27)

        .end    raise_caml_exception
