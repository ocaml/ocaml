type m128d
type m256d

external blank_int : unit -> int = "%asm" "" "" "" "=r"
external __cpuid : int -> int -> int -> int = "%asm" "cpuid"
       "sar	$1, %rax	# __cpuid
	cpuid
	sal	$1, %rax
	orq	$1, %rax
	sal	$1, %rbx
	orq	$1, %rbx
	sal	$1, %rcx
	orq	$1, %rcx
	sal	$1, %rdx
	orq	$1, %rdx" "+a" "+b" "+c" "=d"
let __cpuid a =
  let b = blank_int () in
  let c = blank_int () in
  let d = __cpuid a b c in
  a, b, c, d

(* %ecx *)
let bit_SSE3        = 1 lsl 0
let bit_PCLMUL      = 1 lsl 1
let bit_SSSE3       = 1 lsl 9
let bit_FMA         = 1 lsl 12
let bit_CMPXCHG16B  = 1 lsl 13
let bit_SSE4_1      = 1 lsl 19
let bit_SSE4_2      = 1 lsl 20
let bit_MOVBE       = 1 lsl 22
let bit_POPCNT      = 1 lsl 23
let bit_AES         = 1 lsl 25
let bit_XSAVE       = 1 lsl 26
let bit_OSXSAVE     = 1 lsl 27
let bit_AVX         = 1 lsl 28
let bit_F16C        = 1 lsl 29
let bit_RDRND       = 1 lsl 30

(* %edx *)
let bit_CMPXCHG8B   = 1 lsl 8
let bit_CMOV        = 1 lsl 15
let bit_MMX         = 1 lsl 23
let bit_FXSAVE      = 1 lsl 24
let bit_SSE         = 1 lsl 25
let bit_SSE2        = 1 lsl 26

(* Extended Features *)
(* %ecx *)
let bit_LAHF_LM     = 1 lsl 0
let bit_ABM         = 1 lsl 5
let bit_SSE4a       = 1 lsl 6
let bit_XOP         = 1 lsl 11
let bit_LWP         = 1 lsl 15
let bit_FMA4        = 1 lsl 16
let bit_TBM         = 1 lsl 21

(* %edx *)
let bit_LM          = 1 lsl 29
let bit_3DNOWP      = 1 lsl 30
let bit_3DNOW       = 1 lsl 31

(* Extended Features (%eax == 7) *)
let bit_FSGSBASE    = 1 lsl 0
let bit_BMI         = 1 lsl 3

module Sse2 = struct
  external _mm_add_sd : m128d -> m128d -> m128d = "%asm" ""
       "addsd	%1, %2	# _mm_add_sd" "2" "xm64" "=x"
  external _mm_add_pd : m128d -> m128d -> m128d = "%asm" ""
       "addpd	%1, %2	# _mm_add_pd" "%2" "xm128" "=x"
  external _mm_sub_sd : m128d -> m128d -> m128d = "%asm" ""
       "subsd	%1, %2	# _mm_sub_sd" "2" "xm64" "=x"
  external _mm_sub_pd : m128d -> m128d -> m128d = "%asm" ""
       "subpd	%1, %2	# _mm_sub_pd" "2" "xm128" "=x"
  external _mm_mul_sd : m128d -> m128d -> m128d = "%asm" ""
       "mulsd	%1, %2	# _mm_mul_sd" "2" "xm64" "=x"
  external _mm_mul_pd : m128d -> m128d -> m128d = "%asm" ""
       "mulpd	%1, %2	# _mm_mul_pd" "%2" "xm128" "=x"
  external _mm_div_sd : m128d -> m128d -> m128d = "%asm" ""
       "divsd	%1, %2	# _mm_div_sd" "2" "xm64" "=x"
  external _mm_div_pd : m128d -> m128d -> m128d = "%asm" ""
       "divpd	%1, %2	# _mm_div_pd" "2" "xm128" "=x"
  external _mm_sqrt_sd : m128d -> m128d = "%asm" ""
       "sqrtsd	%0, %1	# _mm_sqrt_sd" "xm64" "=x"
  external _mm_sqrt_pd : m128d -> m128d = "%asm" ""
       "sqrtpd	%0, %1	# _mm_sqrt_pd" "xm128" "=x"
  external _mm_min_sd : m128d -> m128d -> m128d = "%asm" ""
       "minsd	%1, %2	# _mm_min_sd" "2" "xm64" "=x"
  external _mm_min_pd : m128d -> m128d -> m128d = "%asm" ""
       "minpd	%1, %2	# _mm_min_pd" "%2" "xm128" "=x"
  external _mm_max_sd : m128d -> m128d -> m128d = "%asm" ""
       "maxsd	%1, %2	# _mm_max_sd" "2" "xm64" "=x"
  external _mm_max_pd : m128d -> m128d -> m128d = "%asm" ""
       "maxpd	%1, %2	# _mm_max_pd" "%2" "xm128" "=x"
  external _mm_and_pd : m128d -> m128d -> m128d = "%asm" ""
       "andpd	%1, %2	# _mm_and_pd" "%2" "xm128" "=x"
  external _mm_andnot_pd : m128d -> m128d -> m128d = "%asm" ""
       "andnpd	%1, %2	# _mm_andnot_pd" "2" "xm128" "=x"
  external _mm_or_pd : m128d -> m128d -> m128d = "%asm" ""
       "orpd	%1, %2	# _mm_or_pd" "%2" "xm128" "=x"
  external _mm_xor_pd : m128d -> m128d -> m128d = "%asm" ""
       "xorpd	%1, %2	# _mm_xor_pd" "%2" "xm128" "=x"

  external _mm_set_pd : float -> float -> m128d = "%asm" ""
       "unpcklpd	%1, %2	# _mm_set_pd" "2" "xm128" "=x"
  external _mm_cvtsd_f64 : m128d -> float = "%asm" ""
       "xorpd	%1, %1	# _mm_cvtsd_f64
	movsd	%0, %1" "xm64" "=x"
  external _mm_unpackhi_pd : m128d -> m128d -> m128d = "%asm" ""
       "unpckhpd	%1, %2	# _mm_unpackhi_pd" "2" "xm128" "=x"
end

module Avx = struct
  external _mm256_add_pd : m256d -> m256d -> m256d = "%asm" ""
       "vaddpd	%0, %1, %2	# _mm256_add_pd" "x" "xm256" "=x"

  external _mm256_unpacklo_pd : m256d -> m256d -> m256d = "%asm" ""
       "vunpcklpd	%t0, %t1, %t2	# _mm256_unpacklo_pd" "x" "xm256" "=x"
  external _mm256_unpackhi_pd : m256d -> m256d -> m256d = "%asm" ""
       "vunpckhpd	%t0, %t1, %t2	# _mm256_unpackhi_pd" "x" "xm256" "=x"
  external _mm256_castpd128_pd256 : m128d -> m256d = "%asm" ""
       "				# _mm256_castpd128_pd256" "1" "=x"
  external _mm256_castpd256_pd128 : m256d -> m128d = "%asm" ""
       "				# _mm256_castpd256_pd128" "1" "=x"
  let _mm256_set_pd x y z w =
    _mm256_unpacklo_pd
      (_mm256_castpd128_pd256 (Sse2._mm_set_pd x y))
      (_mm256_castpd128_pd256 (Sse2._mm_set_pd z w))
end
