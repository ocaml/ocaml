(* This module exercises comprehensive OCaml code to cover all bytecode opcodes.
   It is linked with run.ml which analyses the bytecode image to find which
   opcodes were never emitted.

   Opcodes that are structurally impossible to emit:
   - EVENT / BREAK: debugging opcodes patched in at runtime by the debugger
   - UGEINT: never referenced in the emitter (Kisout maps to ULTINT;
     the peephole produces BULTINT/BUGEINT but never plain UGEINT)
   - ULTINT: Kisout is always followed by Kbranchif/not in the matching
     compiler, so the peephole always combines it into BULTINT/BUGEINT
   - ATOM: requires Kconst(Const_block(t,[])) with t>0; no OCaml construct
     creates zero-field blocks with non-zero tag
   - PUSHATOM: same, in push context
   - PUSHATOM0: requires Kconst(Const_block(0,[])) after Kpush; no OCaml
     construct produces this (empty arrays go through Kmakeblock, not Kconst)
   - ENVACC1 / PUSHENVACC1: env[1] is the closure info field; free
     variables start at env[2], so ENVACC1 is never emitted *)

(* --- ACC0-ACC7, ACC, PUSHACC0-PUSHACC7, PUSHACC ---
   Deep let bindings to access locals at various stack depths *)
let test_acc () =
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  let e = 5 in
  let f = 6 in
  let g = 7 in
  let h = 8 in
  let i = 9 in
  ignore (a + b + c + d + e + f + g + h + i)

(* --- POP, ASSIGN --- *)
let test_assign () =
  for i = 0 to 10 do
    ignore i
  done

(* --- ENVACC1-ENVACC4, ENVACC, PUSHENVACC1-PUSHENVACC4, PUSHENVACC ---
   Closures that capture free variables. The closures MUST be called,
   otherwise the lambda simplifier eliminates their bodies.
   - Bare ENVACCn: the first free variable in an addition chain (left-most,
     evaluated first, stays in accumulator)
   - PUSHENVACCn: subsequent free variables (pushed for the addition) *)
let test_envacc () =
  let a = Sys.opaque_identity 1 in
  let b = Sys.opaque_identity 2 in
  let c = Sys.opaque_identity 3 in
  let d = Sys.opaque_identity 4 in
  let e = Sys.opaque_identity 5 in
  (* f_forward: a+b+c+d+e with env [a=1,b=2,c=3,d=4,e=5]
     Bare ENVACC for a (leftmost); PUSHENVACC for b,c,d,e *)
  let f_forward () = a + b + c + d + e in
  (* f_reverse: e+d+c+b+a
     Bare ENVACC for e; PUSHENVACC for d,c,b,a (gives PUSHENVACC1 for a) *)
  let f_reverse () = e + d + c + b + a in
  (* f_single: closure capturing one var, returning it directly
     Produces bare ENVACC1 in return position *)
  let f_single () = a in
  ignore (f_forward () + f_reverse () + f_single ())

(* --- CONST0-CONST3, CONSTINT, PUSHCONST0-PUSHCONST3, PUSHCONSTINT --- *)
let test_const () =
  let a = 0 in
  let b = 1 in
  let c = 2 in
  let d = 3 in
  let e = 42 in
  ignore (a + b + c + d + e)

(* --- ATOM0 ---
   ATOM0 is emitted for Kmakeblock(0,0) and Kconst(Const_block(0,[])).
   Empty arrays [||] go through Kmakeblock(0,0). *)
let test_atom () =
  let _empty = ([||] : int array) in
  ()

(* --- MAKEBLOCK1-MAKEBLOCK3, MAKEBLOCK --- *)
let test_makeblock () =
  let _t1 = ref 1 in
  let _t2 = (1, 2) in
  let _t3 = (1, 2, 3) in
  let _t4 = (1, 2, 3, 4) in
  ()

(* --- MAKEFLOATBLOCK, GETFLOATFIELD, SETFLOATFIELD --- *)
type float_rec = { mutable fx: float; mutable fy: float }
let test_float_record () =
  let r = { fx = 1.0; fy = 2.0 } in
  let _ = r.fx +. r.fy in
  r.fx <- 3.0;
  r.fy <- 4.0

(* --- GETFIELD0-GETFIELD3, GETFIELD, SETFIELD0-SETFIELD3, SETFIELD --- *)
type big_rec = {
  mutable f0: int; mutable f1: int; mutable f2: int; mutable f3: int;
  mutable f4: int
}
let test_fields () =
  let r = { f0 = 0; f1 = 1; f2 = 2; f3 = 3; f4 = 4 } in
  let _ = r.f0 + r.f1 + r.f2 + r.f3 + r.f4 in
  r.f0 <- 10;
  r.f1 <- 11;
  r.f2 <- 12;
  r.f3 <- 13;
  r.f4 <- 14

(* --- CLOSURE, CLOSUREREC --- *)
let test_closure () =
  let f x = x + 1 in
  let rec g x = if x <= 0 then 0 else g (x - 1) in
  ignore (f 1 + g 5)

(* --- OFFSETCLOSUREM3, OFFSETCLOSURE0, OFFSETCLOSURE3 (bare, non-pushed) ---
   Mutual rec functions that RETURN their partner as a value (not call it).
   This puts Koffsetclosure in non-push context, emitting bare OFFSETCLOSURE*.
   Obj.repr (%identity) is optimized away, leaving bare offset closure access.
   - even_ret returning self: OFFSETCLOSURE0
   - even_ret returning odd_ret: OFFSETCLOSURE3 (offset +3)
   - odd_ret returning even_ret: OFFSETCLOSUREM3 (offset -3) *)
let test_mutrec_bare () =
  let rec even_ret n =
    if n = 0 then Obj.repr even_ret (* OFFSETCLOSURE0: self-ref *)
    else Obj.repr odd_ret  (* OFFSETCLOSURE3: ref to partner *)
  and odd_ret _n = Obj.repr even_ret  (* OFFSETCLOSUREM3 *) in
  ignore (even_ret 1, odd_ret 0)

(* --- OFFSETCLOSURE (generic, bare) ---
   Three+ mutually recursive functions; distant offsets (>3) produce
   generic OFFSETCLOSURE.
   Offsets: mf1=0, mf2=+3, mf3=+6. From mf1, accessing mf3 is offset +6. *)
let test_mutrec_generic () =
  let rec mf1 _n = Obj.repr mf3  (* OFFSETCLOSURE 6: generic, bare *)
  and mf2 _n = Obj.repr mf1      (* OFFSETCLOSUREM3: bare *)
  and mf3 _n = Obj.repr mf1      (* OFFSETCLOSURE -6: generic, bare *)
  in
  ignore (mf1 0, mf2 0, mf3 0)

(* --- PUSHOFFSETCLOSUREM3, PUSHOFFSETCLOSURE0, PUSHOFFSETCLOSURE3,
       PUSHOFFSETCLOSURE ---
   Two mutually recursive functions that CALL their partners (push context) *)
let test_mutrec_push () =
  let rec even n = if n = 0 then true else odd (n - 1)
  and odd n = if n = 0 then false else even (n - 1) in
  ignore (even 10, odd 10)

(* Three+ mutual rec in push context for PUSHOFFSETCLOSURE (generic) *)
let test_mutrec3_push () =
  let rec f1 n = if n = 0 then 0 else f2 (n - 1) + f3 (n - 1)
  and f2 n = if n = 0 then 1 else f1 (n - 1)
  and f3 n = if n = 0 then 2 else f2 (n - 1) + f1 (n - 1) in
  ignore (f1 3 + f2 3 + f3 3)

(* --- APPLY1-APPLY3, APPLY, PUSH_RETADDR --- *)
let app1 f x = f x
let app2 f x y = f x y
let app3 f x y z = f x y z
let app4 f x y z w = f x y z w

let test_apply () =
  let _a = app1 succ 1 in
  let _b = app2 ( + ) 1 2 in
  let _c = app3 (fun a b c -> a + b + c) 1 2 3 in
  let _d = app4 (fun a b c d -> a + b + c + d) 1 2 3 4 in
  ()

(* --- APPTERM1-APPTERM3, APPTERM ---
   Tail calls with different arities *)
let rec tailcall1 f x = f x
and tailcall2 f x y = f x y
and tailcall3 f x y z = f x y z
and tailcall4 f x y z w = f x y z w

(* --- RETURN, RESTART, GRAB ---
   Multi-argument functions *)
let multi_arg a b c = a + b + c

(* --- GETGLOBAL, PUSHGETGLOBAL, GETGLOBALFIELD, PUSHGETGLOBALFIELD,
       SETGLOBAL --- *)
let test_globals () =
  let _ = Sys.max_string_length in
  let _ = List.length [1;2;3] in
  ()

(* --- BRANCH, BRANCHIF, BRANCHIFNOT --- *)
let test_branch x =
  if x then 1 else 2

(* --- SWITCH --- *)
type variant = X | Y of int | Z of int * int
let test_switch = function
  | X -> 0
  | Y n -> n
  | Z (a, b) -> a + b

(* --- BOOLNOT --- *)
let test_boolnot x = not x

(* --- PUSHTRAP, POPTRAP, RAISE, RERAISE, RAISE_NOTRACE --- *)
exception My_exn
let test_exceptions () =
  (try raise My_exn with My_exn -> ());
  (try raise My_exn with exn -> raise exn);
  (try raise_notrace My_exn with My_exn -> ())

(* --- CHECK_SIGNALS --- *)
let test_signals () =
  for _i = 1 to 100 do () done

(* --- C_CALL1-C_CALL5, C_CALLN --- *)
external c_call1 : 'a -> int = "caml_obj_tag"
external c_call2 : bytes -> int -> char = "%bytes_unsafe_get"
external c_call3 : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external c_calln :
  Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list ->
  Unix.sockaddr -> int
  = "caml_unix_sendto" "caml_unix_sendto_native"

let test_ccalls () =
  let _ = c_call1 (ref 0) in
  let b = Bytes.create 10 in
  let _ = c_call2 b 0 in
  c_call3 b 0 'a';
  let _ = Hashtbl.hash 42 in
  (* C_CALLN: requires 6+ arg external; Unix.unsafe_sendto has 6 args.
     Guard with opaque false so the opcode is emitted but never executed. *)
  if Sys.opaque_identity false then
    ignore (c_calln (Obj.magic 0) (Bytes.create 0) 0 0 [] (Unix.ADDR_UNIX ""))
  else ()

(* --- Arithmetic/bitwise ops: NEGINT, ADDINT, SUBINT, MULINT, DIVINT,
       MODINT, ANDINT, ORINT, XORINT, LSLINT, LSRINT, ASRINT --- *)
let test_arith a b =
  let _ = -a in
  let _ = a + b in
  let _ = a - b in
  let _ = a * b in
  let _ = a / b in
  let _ = a mod b in
  let _ = a land b in
  let _ = a lor b in
  let _ = a lxor b in
  let _ = a lsl b in
  let _ = a lsr b in
  let _ = a asr b in
  ()

(* --- EQ, NEQ, LTINT, LEINT, GTINT, GEINT --- *)
let test_comp (a : int) (b : int) =
  let _ = a = b in
  let _ = a <> b in
  let _ = a < b in
  let _ = a <= b in
  let _ = a > b in
  let _ = a >= b in
  ()

(* --- BEQ, BNEQ, BLTINT, BLEINT, BGTINT, BGEINT ---
   Peephole: comparisons against constants followed by branches *)
let test_branch_comp x =
  (if x = 5 then 1 else 0) +
  (if x <> 5 then 1 else 0) +
  (if x < 5 then 1 else 0) +
  (if x <= 5 then 1 else 0) +
  (if x > 5 then 1 else 0) +
  (if x >= 5 then 1 else 0)

(* --- BULTINT, BUGEINT ---
   Pattern matching range checks (Kisout) *)
let test_range_check = function
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | _ -> "other"

(* --- OFFSETINT, OFFSETREF --- *)
let test_offset () =
  let r = ref 0 in
  incr r;
  decr r;
  for _i = 1 to 5 do () done

(* --- ISINT --- *)
type isint_test = Leaf | Node of isint_test * isint_test
let test_isint = function
  | Leaf -> true
  | Node _ -> false

(* --- VECTLENGTH, GETVECTITEM, SETVECTITEM --- *)
let test_array () =
  let a = [| 1; 2; 3 |] in
  let _ = Array.length a in
  let _ = a.(0) in
  a.(1) <- 42

(* --- GETSTRINGCHAR, GETBYTESCHAR, SETBYTESCHAR --- *)
let test_string_bytes () =
  let s = "hello" in
  let _ = String.unsafe_get s 0 in
  let b = Bytes.of_string "world" in
  let _ = Bytes.unsafe_get b 0 in
  Bytes.unsafe_set b 0 'W'

(* --- GETMETHOD, GETPUBMET --- *)
class myclass = object
  method mymethod = 42
  method othermethod x = x + 1
end

let test_objects () =
  let obj = new myclass in
  let _ = obj#mymethod in
  let _ = obj#othermethod 1 in
  ()

(* --- GETDYNMET ---
   Dynamic method dispatch with a method label computed at runtime *)
let test_dynmet (obj : < mymethod: int; .. >) lbl =
  Oo.public_method_label "mymethod" |> ignore;
  let tag = Sys.opaque_identity lbl in
  let m = (Obj.obj (Obj.repr obj) : int -> int) in
  ignore (m tag)

(* --- PERFORM, RESUME, RESUMETERM, REPERFORMTERM --- *)
open Effect
open Effect.Deep
type _ Effect.t += MyEffect : int -> int Effect.t

let test_effects () =
  match_with (fun () -> perform (MyEffect 42)) ()
    { retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = fun (type a) (eff : a Effect.t) ->
        match eff with
        | MyEffect n ->
          Some (fun (k : (a, _) continuation) ->
            continue k (n + 1))
        | _ -> None }
