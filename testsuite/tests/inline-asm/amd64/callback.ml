type float_ = float
external add1 : float_ -> float_ -> float_ = "%asm" ""
       "movq	add_stub@GOTPCREL(%rip), %rax
	call	caml_c_call@PLT
%call_return
	movq    caml_young_ptr@GOTPCREL(%rip), %r11
	movq    (%r11), %r15
end%=:"
  "D" "S" "=a" "cc" "memory" "%rbp" "%rdi" "%rsi" "%rax" "%rbx" "%rcx" "%rdx" "%r8" "%r9"
  "%r10" "%r11" "%r12" "%r13"
  "%xmm0" "%xmm1" "%xmm2" "%xmm3" "%xmm4" "%xmm5" "%xmm6" "%xmm7" "%xmm8" "%xmm9" "%xmm10"
  "%xmm11" "%xmm12" "%xmm13" "%xmm14" "%xmm15"

external add2 : float -> float -> float = "add_stub"

let () =
  let len = 1_000_000 in
  let x = 1. in
  let s = ref 0. in
  for i = 1 to len do
    s := add1 !s x;
    s := add2 !s x
  done;
  assert (float_of_int (len * 2) = !s)
