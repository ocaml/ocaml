open Intrinsics
open Intrinsics.Sse2
open Intrinsics.Avx

type m128d = Intrinsics.m128d
type m256d = Intrinsics.m256d

let f     = 5.
let i     = 5
let i32   = 5l
let i64   = 5L
let inat  = 5n
let t     = (5, 5)
let rf    = ref 6.
let ri    = ref 6
let ri32  = ref 6l
let ri64  = ref 6L
let rinat = ref 6n
let rt    = ref (6, 6)

external func1a : 'a ref -> 'a                
  = "%asm" "" "mov	(%0), %1	# func1a" "r" "=r"
external func1b : float ref -> 'a
  = "%asm" "" "mov	(%0), %1	# func1b" "r" "=r"
external func1c : int ref -> int
  = "%asm" "" "mov	(%0), %1	# func1c" "r" "=r"
external func1d : int32 ref -> 'a
  = "%asm" "" "mov	(%0), %1	# func1d" "r" "=r"
external func1e : int64 ref -> 'a
  = "%asm" "" "mov	(%0), %1	# func1e" "r" "=r"
external func1f : nativeint ref -> 'a
  = "%asm" "" "mov	(%0), %1	# func1f" "r" "=r"
external func1g : (int * int) ref -> int * int
  = "%asm" "" "mov	(%0), %1	# func1g" "r" "=r"
let () =
  (* Test a memory-register instruction *)
  let x = func1a rf    in assert (x = !rf);
  let x = func1a ri    in assert (x = !ri);
  let x = func1a ri32  in assert (x = !ri32);
  let x = func1a ri64  in assert (x = !ri64);
  let x = func1a rinat in assert (x = !rinat);
  let x = func1a rt    in assert (x = !rt);
  let x = func1b rf    in assert (x = !rf);
  let x = func1c ri    in assert (x = !ri);
  let x = func1d ri32  in assert (x = !ri32);
  let x = func1e ri64  in assert (x = !ri64);
  let x = func1f rinat in assert (x = !rinat);
  let x = func1g rt    in assert (x = !rt);

external func2a : 'a -> 'a ref                   -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2b : 'a -> float ref                -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2c : int -> int ref                 -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2d : 'a -> int32 ref                -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2e : 'a -> int64 ref                -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2f : 'a -> nativeint ref            -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
external func2g : (int * int) -> (int * int) ref -> unit
  = "%asm" "" "mov	%0, (%1)	# func2a" "r" "+r" "=" "memory"
let () =
  (* Test a register-memory instruction *)
  func2a f    rf   ; assert (!rf    = f);
  func2a i    ri   ; assert (!ri    = i);
  func2a i32  ri32 ; assert (!ri32  = i32);
  func2a i64  ri64 ; assert (!ri64  = i64);
  func2a inat rinat; assert (!rinat = inat);
  func2a t    rt   ; assert (!rt    = t);
  rf    := 6.;
  ri    := 6;
  ri32  := 6l;
  ri64  := 6L;
  rinat := 6n;
  rt    := (6, 6);
  func2b f    rf   ; assert (!rf    = f);
  func2c i    ri   ; assert (!ri    = i);
  func2d i32  ri32 ; assert (!ri32  = i32);
  func2e i64  ri64 ; assert (!ri64  = i64);
  func2f inat rinat; assert (!rinat = inat);
  func2g t    rt   ; assert (!rt    = t);
  rf    := 6.;
  ri    := 6;
  ri32  := 6l;
  ri64  := 6L;
  rinat := 6n;
  rt    := (6, 6);

external func3a : 'a -> 'a
  = "%asm" "" "mov	%0, %1	# func3a" "r" "=r"
external func3b : float -> float
  = "%asm" "" "mov	%0, %1	# func3b" "r" "=r"
external func3c : int -> int
  = "%asm" "" "mov	%0, %1	# func3c" "r" "=r"
external func3d : int32 -> int32
  = "%asm" "" "mov	%0, %1	# func3d" "r" "=r"
external func3e : int64 -> int64
  = "%asm" "" "mov	%0, %1	# func3e" "r" "=r"
external func3f : nativeint -> nativeint
  = "%asm" "" "mov	%0, %1	# func3f" "r" "=r"
external func3g : int * int -> int * int
  = "%asm" "" "mov	%0, %1	# func3g" "r" "=r"
let () =
  (* Test a register-register instruction *)
  let x = func3a f    in assert (x = f);
  let x = func3a i    in assert (x = i);
  let x = func3a i32  in assert (x = i32);
  let x = func3a i64  in assert (x = i64);
  let x = func3a inat in assert (x = inat);
  let x = func3a t    in assert (x = t);
  let x = func3b f    in assert (x = f);
  let x = func3c i    in assert (x = i);
  let x = func3d i32  in assert (x = i32);
  let x = func3e i64  in assert (x = i64);
  let x = func3f inat in assert (x = inat);
  let x = func3g t    in assert (x = t);

external func4a : int -> unit
  = "%asm" "" "add	$0x2, %0	# func4a" "+r" "="
external func4b : int32 -> unit
  = "%asm" "" "add	$0x1, %0	# func4b" "+r" "="
external func4c : int64 -> unit
  = "%asm" "" "add	$0x1, %0	# func4c" "+r" "="
external func4d : nativeint -> unit
  = "%asm" "" "add	$0x1, %0	# func4d" "+r" "="
let () =
  (* Test a immediate-register instruction *)
  let x = !ri                   in func4a x; assert (x = 7);
  let x = Int32.add i32 1l      in func4b x; assert (x = 6l);
  let x = Int64.add i64 1L      in func4c x; assert (x = 6L);
  let x = Nativeint.add inat 1n in func4d x; assert (x = 6n);

external func5a : int -> int       = "%asm" ""
       "mov	$0x2, %1	# func5a
	add	%0, %1" "r" "=&r"
external func6a : int -> int       = "%asm" ""
       "mov	$0x2, %1	# func6a
	add	%0, %1" "r" "=r"
let () =
  (* Test earlyclobber *)
  let x = func5a !ri in assert (x = 7);
  let x = func6a !ri in assert (x != 7);

external func7a : int -> int -> int = "%asm" ""
       "add	%0, %1	# func7a
	decq	%1
	mov	%1, %2" "r" "r" "=r"
let () =
  (* Test multiple arguments *)
  let x = func7a !ri !ri in assert (x = 12);

external func8a : int -> int -> int = "%asm" ""
       "add	%1, %2	# func8a
	decq	%2" "2" "r" "=r"
let () =
  (* Test copy to output *)
  let x = func8a i !ri in assert (x = 11);

external func9a : int -> int = "%asm" "" "mov	%0, %1	# func9a" "m" "=r"
let () =
  (* Test memory argument *)
  let x = func9a !ri in assert (x = 6);

external func10a : int -> unit = "%asm" ""
       "inc	%%eax	# func10a
	inc	%%eax" "+a" "" "cc"
let () =
  (* Test exact register EAX argument *)
  let x = !ri in func10a x; assert (x = 7);

external func11a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func11a
	decq	%1" "r" "+d" "" "cc"
let () =
  (* Test exact register RDX argument *)
  let x = !ri in func11a 5 x; assert (x = 11);

external func12a : int -> int -> int -> int = "%asm" ""
       "sar	$1, %1	# func12a
	xorq	$1, %2
	mul	%1
	orq	$1, %2
	shl	$1, %3
	orq	$1, %3" "%2" "r" "=a" "=d" "cc"
let () =
  (* Test multiple outputs and commutative inputs *)
  let x = !ri in
  let y = !ri in
  let z = !ri in
  let w = func12a x y z in
  assert (z = 36);
  assert (w = 0);

external func12b : float -> float -> float
  = "%asm" "" "addsd	%1, %2	# func12b" "2" "mx" "=x"
external func12c : float -> float -> float
  = "%asm" "" "addsd	%1, %2	# func12c" "%2" "mx" "=x"
let () =
  (* Test commutative inputs *)
  let x = !rf +. 1. in let z = func12b !rf x in assert (z = 13.);
  let x = !rf +. 1. in let z = func12c !rf x in assert (z = 13.);

external func13a : int -> int -> int -> int = "%asm" ""
       "sar	$1, %1	# func 13a
	xorq	$1, %2
	mul	%1
	orq	$1, %2
	shl	$1, %3
	orq	$1, %3" "%2" "D" "=a" "=d" "cc"
let () =
  (* Test multiple outputs, commutative inputs and an exact input argument *)
  let x = !ri in
  let y = !ri in
  let z = !ri in
  let w = func13a x y z in
  assert (z = 36);
  assert (w = 0);

external func14a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func14a
	decq	%1" "rm" "+r" "" "cc"
let () =
  (* Test multiple memory-register constraint *)
  let x = !ri in let y = !ri in func14a x y; assert (y = 12);
  let x = !ri in func14a !ri x; assert (x = 12);

external func15a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func15a
	decq	%1" "g" "+r" "" "cc"
let () =
  (* Test multiple immediate-memory-register constraint *)
  let x = !ri in let y = !ri in func15a x y; assert (y = 12);
  let x = !ri in func15a !ri x; assert (x = 12);
  let x = !ri in func15a 6 x; assert (x = 12);

external func16a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func16a
	decq	%1" "i" "+r" "" "cc"
external func16b : int64 -> int64 -> unit = "%asm" ""
       "add	%0, %1	# func16n" "i" "+r" "" "cc"
let () =
  (* Test immediate argument *)
  let x = !ri in func16a 6 x; assert (x = 12);
  let x = Int64.add !ri64 1L in func16b 6L x; assert (x = 13L);
 
external func17a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func17a
	decq	%1" "m#hello" "+r" "" "cc"
let () =
  (* Test constraint comments *)
  let x = !ri in func17a !ri x; assert (x = 12);

external func18a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func18a
	decq	%1" "r,m,r" "+m,r,r" ",," "memory" "cc"
let () =
  (* Test multiple constraint alternatives *)
  let x = !ri in let y = !ri in func18a x y; assert (y = 12);
  let x = !ri in func18a !ri x; assert (x = 12);
  let x = !ri in func18a x !ri; assert (!ri = 12);
  ri := 6

(* func19 and slight disparagement weren't implemented because how it works is
   unclear. *)

external func20a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func20a
	decq	%1" "d?,r" "+r?,a" "," "memory" "cc"
external func20b : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func20b
	decq	%1" "d,r?" "+r,a?" "," "memory" "cc"
let () =
  (* Test slight alternative disparagement *)
  let x = !ri in let y = !ri in func20a x y; assert (y = 12);
  let x = !ri in let y = !ri in func20b x y; assert (y = 12);

external func21a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func21a
	decq	%1" "d??,r?" "+r??,a?" "," "memory" "cc"
external func21b : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func21b
	decq	%1" "d?,r??" "+r?,a??" "," "memory" "cc"
let () =
  (* Test multiple slight alternative disparagement *)
  let x = !ri in let y = !ri in func21a x y; assert (y = 12);
  let x = !ri in let y = !ri in func21b x y; assert (y = 12);

external func22a : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func22a
	decq	%1" "d!,r??" "+r!,a??" "," "memory" "cc"
external func22b : int -> int -> unit = "%asm" ""
       "add	%0, %1	# func22b
	decq	%1" "d??,r!" "+r??,a!" "," "memory" "cc"
let () =
  (* Test severe alternative disparagement *)
  let x = !ri in let y = !ri in func22a x y; assert (y = 12);
  let x = !ri in let y = !ri in func22b x y; assert (y = 12);

external func23a : int -> int -> unit = "%asm" ""
       "sar	$1, %0	# func23a
	xorq	$1, %1
	mul	%0
	orq	$1, %1
	shl	$1, %0
	orq	$1, %0" "g" "+a" "" "%rdx" "cc"
let () =
  (* Test register clobber *)
  let x = !ri in let y = !ri in func23a x y; assert (y = 36);

external func24a : int -> int -> unit = "%asm" ""
       "mov	%0, %%rdi	# func24a
	call	foo
        jmp	next
foo:	sar	$1, %%rdi
	xorq	$1, %1
	mul	%%rdi
	orq	$1, %1
	ret
next:	" "g" "+a" "" "%rdi" "%rsi" "%rcx" "%r8" "%r9" "memory" "cc"
let () =
  (* Test multiple register clobber *)
  let x = !ri in let y = !ri in func24a x y; assert (y = 36);

external func25a : int -> int -> int -> int -> int = "%asm" ""
       "mov	%0, %2	# func25a
	mov	%1, %3
	shr	$10, %2
	shl	$10, %3
	add	%0, %2
	lea	(%1, %3, 1), %4
	xor	%2, %4
	orq	$1, %4" "g" "g" "=&r" "=&r" "=r" "cc"
let () =
  (* Test explicit temporaries *)
  let x = !ri in
  let y = !ri in
  let z = !ri in
  let w = !ri in
  let u = func25a x y z w in
  assert (u = 6656);

(* Explicit register naming is not implemented *)
(* Less common constraint types are not implemented *)

external func35a : char -> char = "%asm" ""
       "xor	%1, %1	# func35a
	mov	%b0, %b1" "d" "=a"
external func35b : char -> char = "%asm" ""
       "xor	%1, %1	# func35b
	mov	%h0, %h1" "d" "=a"
let () =
  (* Test operand modifiers 'b' and 'h' *)
  let a = func35a 'a' in assert (a = 'a');
  let a = func35b 'a' in assert (a <> 'a');

external func36a : int -> int -> int -> int -> int = "%asm" ""
       "mov	%0, %4	# func36a
	add	%1, %2
        add	%3, %4
        add	%2, %4
	orq	$1, %4" "a" "b" "c" "d" "=&r" "cc"
let () =
  (* Test register operands a, b, c and a *)
  let x = !ri in
  let y = !ri + 1 in
  let z = !ri + 2 in
  let w = !ri + 3 in
  let u = func36a x y z w in
  assert (u = 32)

external func37a : int -> int -> int = "%asm" ""
       "mov	%0, %2	# func37a
	add	%1, %2
        subq	$1, %2" "D" "S" "=&r" "cc"
let () =
  (* Test register operands S and D *)
  let x = !ri in
  let y = !ri + 1 in
  let z = func37a x y in
  assert (z = 13)

(* X87 and MMX registers are currently not supported *)

external func43a : m128d -> m128d -> unit
  = "%asm" "" "addpd	%0, %1	# func43a" "x" "+&x" ""
let () =
  (* Test XMM registers *)
  let x = _mm_set_pd 1. 2. in
  let y = _mm_set_pd 3. 4. in
  func43a x y;
  assert (_mm_cvtsd_f64 y = 4.);
  assert (_mm_cvtsd_f64 (_mm_unpackhi_pd y y) = 6.);

(* Integer constraints are currently not supported *)

external func55a : string -> unit
  = "%asm" "" "mov%B0	$1, (%0)	# func55a" "r" "" "memory"
let () =
  (* Test 'B' operand modifier *)
  let s = "abcdefgh" in
  func55a s;
  assert (s = "\001bcdefgh")

external func56a : string -> unit
  = "%asm" "" "mov%W0	$1, (%0)	# func56a" "r" "" "memory"
let () =
  (* Test 'W' operand modifier *)
  let s = "abcdefgh" in
  func56a s;
  assert (s = "\001\000cdefgh")

external func57a : string -> unit
  = "%asm" "" "mov%L0	$1, (%0)	# func57a" "r" "" "memory"
let () =
  (* Test 'L' operand modifier *)
  let s = "abcdefgh" in
  func57a s;
  assert (s = "\001\000\000\000efgh")

(* Labels are not supported *)

external func67a : int64 -> string -> unit
  = "%asm" "" "mov	%b0, (%1)	# func67a" "r" "r" "" "memory"
let () =
  (* Test 'b' operand modifier *)
  let s = "abcdefgh" in
  func67a 0x41L s;
  assert (s = "Abcdefgh")

external func68a : int64 -> string -> unit
  = "%asm" "" "mov	%w0, (%1)	# func68a" "r" "r" "" "memory"
let () =
  (* Test 'w' operand modifier *)
  let s = "abcdefgh" in
  func68a 0x4241L s;
  assert (s = "ABcdefgh")

external func69a : int64 -> string -> unit
  = "%asm" "" "mov	%k0, (%1)	# func69a" "r" "r" "" "memory"
let () =
  (* Test 'k' operand modifier *)
  let s = "abcdefgh" in
  func69a 0x44434241L s;
  assert (s = "ABCDefgh")

external func70a : int64 -> string -> unit
  = "%asm" "" "mov	%q0, (%1)	# func70a" "r" "r" "" "memory"
let () =
  (* Test 'q' operand modifier *)
  let s = "abcdefgh" in
  func70a 0x4847464544434241L s;
  assert (s = "ABCDEFGH")

external func71a : int64 -> string -> unit
  = "%asm" "" "mov	%h0, (%1)	# func71a" "a" "r" "" "memory"
let () =
  (* Test 'h' operand modifier *)
  let s = "abcdefgh" in
  func71a 0x4241L s;
  assert (s = "Bbcdefgh")

let supports_avx =
  let _, _, c, _ = __cpuid 1 in
  c land bit_AVX <> 0

external func78a : m128d -> m256d = "%asm" ""
       "vmovapd	%t0, %t1	# func78" "x" "=x"
let () =
  if supports_avx then begin
    let x = _mm_set_pd 1. 2. in
    let y = func78a x in
    assert (_mm256_castpd256_pd128 y = x)
  end

external func79a : m256d -> m128d = "%asm" ""
       "movapd	%x0, %x1	# func79" "x" "=x"
let () =
  if supports_avx then begin
    let x = _mm256_set_pd 1. 2. 3. 4. in
    let y = func79a x in
    assert (_mm256_castpd256_pd128 x = y)
  end


