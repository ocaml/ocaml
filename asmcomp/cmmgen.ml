(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from closed lambda to C-- *)

open Misc
open Arch
open Asttypes
open Primitive
open Typedtree
open Lambda
open Clambda
open Cmm

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar _ | Cconst_int _ | Cconst_symbol _ | Cconst_pointer _ -> fn arg
  | _ -> let id = Ident.new name in Clet(id, arg, fn (Cvar id))

(* Block headers. Meaning of the tag field:
       0 - 248: regular blocks
       249: infix closure
       250: closures
       251: abstract
       252: string
       253: float
       254: float array
       255: finalized *)

let float_tag = Cconst_int 253
let floatarray_tag = Cconst_int 254

let block_header tag sz = (sz lsl 10) + tag
let closure_header sz = block_header 250 sz
let infix_header ofs = block_header 249 ofs
let float_header = block_header 253 (size_float / size_addr)
let floatarray_header len = block_header 254 (len * size_float / size_addr)
let string_header len = block_header 252 ((len + size_addr) / size_addr)

let alloc_block_header tag sz = Cconst_int(block_header tag sz)
let alloc_floatarray_header len = Cconst_int(floatarray_header len)
let alloc_closure_header sz = Cconst_int(closure_header sz)
let alloc_infix_header ofs = Cconst_int(infix_header ofs)

(* Integers *)

let int_const n = Cconst_int((n lsl 1) + 1)

let add_const c n =
  if n = 0 then c else Cop(Caddi, [c; Cconst_int n])

let incr_int = function
    Cconst_int n -> Cconst_int(n+1)
  | Cop(Caddi, [c; Cconst_int n]) -> add_const c (n+1)
  | c -> add_const c 1

let decr_int = function
    Cconst_int n -> Cconst_int(n-1)
  | Cop(Caddi, [c; Cconst_int n]) -> add_const c (n-1)
  | c -> add_const c (-1)

let add_int c1 c2 =
  match (c1, c2) with
    (Cop(Caddi, [c1; Cconst_int n1]),
     Cop(Caddi, [c2; Cconst_int n2])) ->
      add_const (Cop(Caddi, [c1; c2])) (n1 + n2)
  | (Cop(Caddi, [c1; Cconst_int n1]), c2) ->
      add_const (Cop(Caddi, [c1; c2])) n1
  | (c1, Cop(Caddi, [c2; Cconst_int n2])) ->
      add_const (Cop(Caddi, [c1; c2])) n2
  | (c1, c2) ->
      Cop(Caddi, [c1; c2])

let sub_int c1 c2 =
  match (c1, c2) with
    (Cop(Caddi, [c1; Cconst_int n1]),
     Cop(Caddi, [c2; Cconst_int n2])) ->
      add_const (Cop(Csubi, [c1; c2])) (n1 - n2)
  | (Cop(Caddi, [c1; Cconst_int n1]), c2) ->
      add_const (Cop(Csubi, [c1; c2])) n1
  | (c1, Cop(Caddi, [c2; Cconst_int n2])) ->
      add_const (Cop(Csubi, [c1; c2])) (-n2)
  | (c1, Cconst_int n) ->
      add_const c1 (-n)
  | (c1, c2) ->
      Cop(Csubi, [c1; c2])

let tag_int = function
    Cconst_int n -> Cconst_int((n lsl 1) + 1)
  | c -> Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1])

let untag_int = function
    Cconst_int n -> Cconst_int(n asr 1)
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) -> c
  | Cop(Clsl, [c; Cconst_int 1]) -> c
  | c -> Cop(Casr, [c; Cconst_int 1])

(* Bool *)

let test_bool = function
    Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) -> c
  | Cop(Clsl, [c; Cconst_int 1]) -> c
  | c -> Cop(Ccmpi Cne, [c; Cconst_int 1])

(* Float *)

let box_float c = Cop(Calloc, [Cconst_int(float_header); c])

let unbox_float = function
    Cop(Calloc, [header; c]) -> c
  | c -> Cop(Cload typ_float, [c])

let is_unboxed_float = function
    Uconst(Const_base(Const_float f)) -> true
  | Uprim(p, _) ->
      begin match p with
          Pccall p -> p.prim_native_float
        | Pfloatfield _ | Pfloatofint | Pnegfloat
        | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
        | Parrayrefu Pfloatarray | Parrayrefs Pfloatarray -> true
        | _ -> false
      end
  | _ -> false

let subst_boxed_float boxed_id unboxed_id exp =
  let need_boxed = ref false in
  let rec subst = function
      Cvar id as e ->
        if Ident.same id boxed_id then need_boxed := true; e
    | Clet(id, arg, body) -> Clet(id, subst arg, subst body)
    | Cassign(id, arg) -> Cassign(id, subst arg)
    | Ctuple argl -> Ctuple(List.map subst argl)
    | Cop(Cload _, [Cvar id]) as e ->
        if Ident.same id boxed_id then Cvar unboxed_id else e
    | Cop(op, argl) -> Cop(op, List.map subst argl)
    | Csequence(e1, e2) -> Csequence(subst e1, subst e2)
    | Cifthenelse(e1, e2, e3) -> Cifthenelse(subst e1, subst e2, subst e3)
    | Cswitch(arg, index, cases) ->
        Cswitch(subst arg, index, Array.map subst cases)
    | Cloop e -> Cloop(subst e)
    | Ccatch(e1, e2) -> Ccatch(subst e1, subst e2)
    | Ctrywith(e1, id, e2) -> Ctrywith(subst e1, id, subst e2)
    | e -> e in
  let res = subst exp in
  (res, !need_boxed)  

(* Unit *)

let return_unit c = Csequence(c, Cconst_int 1)

let rec remove_unit = function
    Cconst_int 1 -> Ctuple []
  | Csequence(c, Cconst_int 1) -> c
  | Csequence(c1, c2) ->
      Csequence(c1, remove_unit c2)
  | Cifthenelse(cond, ifso, ifnot) ->
      Cifthenelse(cond, remove_unit ifso, remove_unit ifnot)
  | Cswitch(sel, index, cases) ->
      Cswitch(sel, index, Array.map remove_unit cases)
  | Ccatch(body, handler) ->
      Ccatch(remove_unit body, remove_unit handler)
  | Ctrywith(body, exn, handler) ->
      Ctrywith(remove_unit body, exn, remove_unit handler)
  | c -> c

(* Access to block fields *)

let field_address ptr n =
  if n = 0
  then ptr
  else Cop(Cadda, [ptr; Cconst_int(n * size_addr)])

let get_field ptr n =
  Cop(Cload typ_addr, [field_address ptr n])

let set_field ptr n newval =
  Cop(Cstore, [field_address ptr n; newval])

let header ptr =
  Cop(Cload typ_int, [Cop(Cadda, [ptr; Cconst_int(-size_int)])])

let tag_offset =
  if big_endian then -1 else -size_int

let get_tag ptr =
  if Proc.word_addressed then           (* If byte loads are slow *)
    Cop(Cand, [header ptr; Cconst_int 255])
  else                                  (* If byte loads are efficient *)
    Cop(Cloadchunk Byte_unsigned,
        [Cop(Cadda, [ptr; Cconst_int(tag_offset)])])

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr
let log2_size_float = Misc.log2 size_float

let wordsize_shift = 9
let numfloat_shift = 9 + log2_size_float - log2_size_addr

let is_addr_array hdr =
  Cop(Ccmpi Cne, [Cop(Cand, [hdr; Cconst_int 255]); floatarray_tag])

let addr_array_length hdr = Cop(Clsr, [hdr; Cconst_int wordsize_shift])
let float_array_length hdr = Cop(Clsr, [hdr; Cconst_int numfloat_shift])

let lsl_const c n =
  Cop(Clsl, [c; Cconst_int n])

let array_indexing log2size ptr ofs =
  match ofs with
    Cconst_int n ->
      let i = n asr 1 in
      if i = 0 then ptr else Cop(Cadda, [ptr; Cconst_int(i lsl log2size)])
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) ->
      Cop(Cadda, [ptr; lsl_const c log2size])
  | Cop(Caddi, [c; Cconst_int n]) ->
      Cop(Cadda, [Cop(Cadda, [ptr; lsl_const c (log2size - 1)]);
                  Cconst_int((n-1) lsl (log2size - 1))])
  | _ ->
      Cop(Cadda, [Cop(Cadda, [ptr; lsl_const ofs (log2size - 1)]);
                  Cconst_int((-1) lsl (log2size - 1))])

let addr_array_ref arr ofs =
  Cop(Cload typ_addr, [array_indexing log2_size_addr arr ofs])
let unboxed_float_array_ref arr ofs =
  Cop(Cload typ_float, [array_indexing log2_size_float arr ofs])
let float_array_ref arr ofs =
  box_float(unboxed_float_array_ref arr ofs)

let addr_array_set arr ofs newval =
  Cop(Cextcall("modify", typ_void, false),
      [array_indexing log2_size_addr arr ofs; newval])
let int_array_set arr ofs newval =
  Cop(Cstore, [array_indexing log2_size_addr arr ofs; newval])
let float_array_set arr ofs newval =
  Cop(Cstore, [array_indexing log2_size_float arr ofs; newval])

(* String length *)

let string_length exp =
  bind "str" exp (fun str ->
    let tmp_var = Ident.new "tmp" in
    Clet(tmp_var,
         Cop(Csubi,
             [Cop(Clsl,
                  [Cop(Clsr, [get_field str (-1); Cconst_int 10]);
                   Cconst_int log2_size_addr]);
              Cconst_int 1]),
         Cop(Csubi,
             [Cvar tmp_var;
              Cop(Cloadchunk Byte_unsigned,
                  [Cop(Cadda, [str; Cvar tmp_var])])])))

(* To compile "let rec" over values *)

let fundecls_size fundecls =
  let sz = ref (-1) in
  List.iter
    (fun (label, arity, params, body) ->
      sz := !sz + 1 + (if arity = 1 then 2 else 3))
    fundecls;
  !sz

let rec expr_size = function
    Uclosure(fundecls, clos_vars) ->
      fundecls_size fundecls + List.length clos_vars
  | Uprim(Pmakeblock tag, args) ->
      List.length args
  | Ulet(id, exp, body) ->
      expr_size body
  | _ ->
      fatal_error "Cmmgen.expr_size"

let dummy_block size =
  let rec init_val i =
    if i >= size then [] else Cconst_int 0 :: init_val(i+1) in
  Cop(Calloc, alloc_block_header 0 size :: init_val 0)

let rec store_contents ptr = function
    Cop(Calloc, fields) ->
      Cop(Cstore, field_address ptr (-1) :: fields)
  | Clet(id, exp, body) ->
      Clet(id, exp, store_contents ptr body)
  | _ ->
      fatal_error "Cmmgen.store_contents"

(* Record application and currying functions *)

let apply_function n =
  Compilenv.need_apply_fun n; "caml_apply" ^ string_of_int n
let curry_function n =
  Compilenv.need_curry_fun n; "caml_curry" ^ string_of_int n

(* Comparisons *)

let transl_comparison = function
    Lambda.Ceq -> Ceq
  | Lambda.Cneq -> Cne
  | Lambda.Cge -> Cge
  | Lambda.Cgt -> Cgt
  | Lambda.Cle -> Cle
  | Lambda.Clt -> Clt

(* Translate structured constants *)

let const_label = ref 0

let new_const_label () =
  incr const_label;
  !const_label

let new_const_symbol () =
  incr const_label;
  Compilenv.current_unit_name () ^ "_" ^ string_of_int !const_label

let structured_constants = ref ([] : (string * structured_constant) list)

let transl_constant = function
    Const_base(Const_int n) ->
      Cconst_int((n lsl 1) + 1)
  | Const_base(Const_char c) ->
      Cconst_int(((Char.code c) lsl 1) + 1)
  | Const_pointer n ->
      Cconst_pointer((n lsl 1) + 1)
  | cst ->
      let lbl = new_const_symbol() in
      structured_constants := (lbl, cst) :: !structured_constants;
      Cconst_symbol lbl

(* Translate an expression *)

let functions = (Queue.new() : (string * Ident.t list * ulambda) Queue.t)

let rec transl = function
    Uvar id ->
      Cvar id
  | Uconst sc ->
      transl_constant sc
  | Uclosure(fundecls, clos_vars) ->
      let block_size =
        fundecls_size fundecls + List.length clos_vars in
      let rec transl_fundecls pos = function
        [] ->
          List.map transl clos_vars
      | (label, arity, params, body) :: rem ->
          Queue.add (label, params, body) functions;
          let header =
            if pos = 0
            then alloc_closure_header block_size
            else alloc_infix_header pos in
          if arity = 1 then
            header ::
            Cconst_symbol label ::
            int_const 1 ::
            transl_fundecls (pos + 3) rem
          else
            header ::
            Cconst_symbol(curry_function arity) ::
            int_const arity ::
            Cconst_symbol label ::
            transl_fundecls (pos + 4) rem in
      Cop(Calloc, transl_fundecls 0 fundecls)
  | Uoffset(arg, offset) ->
      field_address (transl arg) offset
  | Udirect_apply(lbl, args) ->
      Cop(Capply typ_addr, Cconst_symbol lbl :: List.map transl args)
  | Ugeneric_apply(clos, [arg]) ->
      bind "fun" (transl clos) (fun clos ->
        Cop(Capply typ_addr, [get_field clos 0; transl arg; clos]))
  | Ugeneric_apply(clos, args) ->
      let arity = List.length args in
      Cop(Capply typ_addr,
          Cconst_symbol(apply_function arity) ::
          List.map transl (args @ [clos]))
  | Ulet(id, exp, body) ->
      if is_unboxed_float exp then
        let unboxed_id = Ident.new (Ident.name id) in
        let (tr_body, need_boxed) =
          subst_boxed_float id unboxed_id (transl body) in
        Clet(unboxed_id, transl_unbox_float exp,
             if need_boxed
             then Clet(id, box_float(Cvar unboxed_id), tr_body)
             else tr_body)
      else
        Clet(id, transl exp, transl body)
  | Uletrec(bindings, body) ->
      transl_letrec bindings (transl body)

  (* Primitives *)
  | Uprim(Pidentity, [arg]) ->
      transl arg
  | Uprim(Pgetglobal id, []) ->
      Cconst_symbol(Ident.name id)

  (* Heap blocks *)
  | Uprim(Pmakeblock tag, []) ->
      transl_constant(Const_block(tag, []))
  | Uprim(Pmakeblock tag, args) ->
      Cop(Calloc, alloc_block_header tag (List.length args) ::
                  List.map transl args)
  | Uprim(Pfield n, [arg]) ->
      get_field (transl arg) n
  | Uprim(Psetfield(n, ptr), [loc; newval]) ->
      if ptr then
        return_unit(Cop(Cextcall("modify", typ_void, false),
                        [field_address (transl loc) n; transl newval]))
      else
        return_unit(set_field (transl loc) n (transl newval))
  | Uprim(Pfloatfield n, [arg]) ->
      let ptr = transl arg in
      box_float(Cop(Cload typ_float,
                [if n = 0 then ptr
                          else Cop(Cadda, [ptr; Cconst_int(n * size_float)])]))
  | Uprim(Psetfloatfield n, [loc; newval]) ->
      let ptr = transl loc in
      return_unit(Cop(Cstore,
                  [if n = 0 then ptr
                            else Cop(Cadda, [ptr; Cconst_int(n * size_float)]);
                   transl newval]))

  (* External call *)
  | Uprim(Pccall prim, args) ->
      if prim.prim_native_float then
        box_float
          (Cop(Cextcall(prim.prim_native_name, typ_float, false),
               List.map transl_unbox_float args))
      else begin
        let name =
          if prim.prim_native_name <> ""
          then prim.prim_native_name
          else prim.prim_name in
        Cop(Cextcall(name, typ_addr, prim.prim_alloc), List.map transl args)
      end
  (* Exceptions *)
  | Uprim(Praise, [arg]) ->
      Cop(Craise, [transl arg])

  (* Boolean operations *)
  | Uprim(Psequand, [arg1; arg2]) ->
      Cifthenelse(test_bool(transl arg1), transl arg2, Cconst_int 1)
  | Uprim(Psequor, [arg1; arg2]) ->
      Cifthenelse(test_bool(transl arg1), Cconst_int 3, transl arg2)
  | Uprim(Pnot, [arg]) ->
      Cop(Csubi, [Cconst_int 4; transl arg]) (* 1 -> 3, 3 -> 1 *)

  (* Integer operations *)
  | Uprim(Pnegint, [arg]) ->
      Cop(Csubi, [Cconst_int 2; transl arg])
  | Uprim(Paddint, [arg1; arg2]) ->
      decr_int(add_int (transl arg1) (transl arg2))
  | Uprim(Psubint, [arg1; arg2]) ->
      incr_int(sub_int (transl arg1) (transl arg2))
  | Uprim(Pmulint, [arg1; arg2]) ->
      incr_int(Cop(Cmuli, [decr_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Pdivint, [arg1; arg2]) ->
      tag_int(Cop(Cdivi, [untag_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Pmodint, [arg1; arg2]) ->
      tag_int(Cop(Cmodi, [untag_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Pandint, [arg1; arg2]) ->
      Cop(Cand, [transl arg1; transl arg2])
  | Uprim(Porint, [arg1; arg2]) ->
      Cop(Cor, [transl arg1; transl arg2])
  | Uprim(Pxorint, [arg1; arg2]) ->
      incr_int(Cop(Cxor, [transl arg1; transl arg2]))
  | Uprim(Plslint, [arg1; arg2]) ->
      incr_int(Cop(Clsl, [decr_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Plsrint, [arg1; arg2]) ->
      Cop(Cor, [Cop(Clsr, [decr_int(transl arg1); untag_int(transl arg2)]);
                Cconst_int 1])
  | Uprim(Pasrint, [arg1; arg2]) ->
      Cop(Cor, [Cop(Casr, [decr_int(transl arg1); untag_int(transl arg2)]);
                Cconst_int 1])
  | Uprim(Pintcomp cmp, [arg1; arg2]) ->
      tag_int(Cop(Ccmpi(transl_comparison cmp), [transl arg1; transl arg2]))
  | Uprim(Poffsetint n, [arg]) ->
      add_const (transl arg) (n lsl 1)
  | Uprim(Poffsetref n, [arg]) ->
      return_unit
        (bind "ref" (transl arg) (fun arg ->
          Cop(Cstore,
              [arg; add_const (Cop(Cload typ_int, [arg])) (n lsl 1)])))

  (* Float operations *)
  | Uprim(Pfloatofint, [arg]) ->
      box_float(Cop(Cfloatofint, [untag_int(transl arg)]))
  | Uprim(Pintoffloat, [arg]) ->
     tag_int(Cop(Cintoffloat, [transl_unbox_float arg]))
  | Uprim(Pnegfloat, [arg]) ->
      box_float(Cop(Csubf, [Cconst_float "0.0";
                            transl_unbox_float arg]))
  | Uprim(Paddfloat, [arg1; arg2]) ->
      box_float(Cop(Caddf, [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Uprim(Psubfloat, [arg1; arg2]) ->
      box_float(Cop(Csubf, [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Uprim(Pmulfloat, [arg1; arg2]) ->
      box_float(Cop(Cmulf, [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Uprim(Pdivfloat, [arg1; arg2]) ->
      box_float(Cop(Cdivf, [transl_unbox_float arg1; transl_unbox_float arg2]))
  | Uprim(Pfloatcomp cmp, [arg1; arg2]) ->
      tag_int(Cop(Ccmpf(transl_comparison cmp),
                  [transl_unbox_float arg1; transl_unbox_float arg2]))

  (* String operations *)
  | Uprim(Pstringlength, [arg]) ->
      tag_int(string_length (transl arg))
  | Uprim(Pstringrefu, [arg1; arg2]) ->
      tag_int(Cop(Cloadchunk Byte_unsigned,
                  [add_int (transl arg1) (untag_int(transl arg2))]))
  | Uprim(Pstringsetu, [arg1; arg2; arg3]) ->
      return_unit(Cop(Cstorechunk Byte_unsigned,
                      [add_int (transl arg1) (untag_int(transl arg2));
                       untag_int(transl arg3)]))
  | Uprim(Pstringrefs, [arg1; arg2]) ->
      tag_int
        (bind "str" (transl arg1) (fun str ->
          bind "index" (untag_int (transl arg2)) (fun idx ->
            Csequence(
              Cop(Ccheckbound, [string_length str; idx]),
              Cop(Cloadchunk Byte_unsigned, [add_int str idx])))))
  | Uprim(Pstringsets, [arg1; arg2; arg3]) ->
      return_unit
        (bind "str" (transl arg1) (fun str ->
          bind "index" (untag_int (transl arg2)) (fun idx ->
            Csequence(
              Cop(Ccheckbound, [string_length str; idx]),
              Cop(Cstorechunk Byte_unsigned,
                  [add_int str idx; untag_int(transl arg3)])))))

  (* Array operations *)
  | Uprim(Pmakearray kind, []) ->
      transl_constant(Const_block(0, []))
  | Uprim(Pmakearray kind, args) ->
      begin match kind with
        Pgenarray ->
          Cop(Cextcall("make_array", typ_addr, true),
              [Cop(Calloc, alloc_block_header 0 (List.length args) ::
                           List.map transl args)])
      | Paddrarray | Pintarray ->
          Cop(Calloc, alloc_block_header 0 (List.length args) ::
                      List.map transl args)
      | Pfloatarray ->
          Cop(Calloc, alloc_floatarray_header (List.length args) ::
                      List.map transl_unbox_float args)
      end
  | Uprim(Parraylength kind, [arg]) ->
      begin match kind with
        Pgenarray ->
          let len =
            if wordsize_shift = numfloat_shift then
              Cop(Clsr, [header(transl arg); Cconst_int wordsize_shift])
            else
              bind "header" (header(transl arg)) (fun hdr ->
                Cifthenelse(is_addr_array hdr,
                            Cop(Clsr, [hdr; Cconst_int wordsize_shift]),
                            Cop(Clsr, [hdr; Cconst_int numfloat_shift]))) in
          Cop(Cor, [len; Cconst_int 1])
      | Paddrarray | Pintarray ->
          Cop(Cor, [addr_array_length(header(transl arg)); Cconst_int 1])
      | Pfloatarray ->
          Cop(Cor, [float_array_length(header(transl arg)); Cconst_int 1])
      end
  | Uprim(Parrayrefu kind, [arg1; arg2]) ->
      begin match kind with
        Pgenarray ->
          bind "arr" (transl arg1) (fun arr ->
            bind "index" (transl arg2) (fun idx ->
              Cifthenelse(is_addr_array(header arr),
                          addr_array_ref arr idx,
                          float_array_ref arr idx)))
      | Paddrarray | Pintarray ->
          addr_array_ref (transl arg1) (transl arg2)
      | Pfloatarray ->
          float_array_ref (transl arg1) (transl arg2)
      end
  | Uprim(Parraysetu kind, [arg1; arg2; arg3]) ->
      return_unit(begin match kind with
        Pgenarray ->
          bind "newval" (transl arg3) (fun newval ->
            bind "index" (transl arg2) (fun index ->
              bind "arr" (transl arg1) (fun arr ->
                Cifthenelse(is_addr_array(header arr),
                            addr_array_set arr index newval,
                            float_array_set arr index (unbox_float newval)))))
      | Paddrarray ->
          addr_array_set (transl arg1) (transl arg2) (transl arg3)
      | Pintarray ->
          int_array_set (transl arg1) (transl arg2) (transl arg3)
      | Pfloatarray ->
          float_array_set (transl arg1) (transl arg2) (transl_unbox_float arg3)
      end)
  | Uprim(Parrayrefs kind, [arg1; arg2]) ->
      begin match kind with
        Pgenarray ->
          bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              bind "header" (header arr) (fun hdr ->
                Cifthenelse(is_addr_array hdr,
                  Csequence(Cop(Ccheckbound, [addr_array_length hdr; idx]),
                            addr_array_ref arr idx),
                  Csequence(Cop(Ccheckbound, [float_array_length hdr; idx]),
                            float_array_ref arr idx)))))
      | Paddrarray | Pintarray ->
          bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              Csequence(Cop(Ccheckbound, [addr_array_length(header arr); idx]),
                        addr_array_ref arr idx)))
      | Pfloatarray ->
          box_float(
            bind "index" (transl arg2) (fun idx ->
              bind "arr" (transl arg1) (fun arr ->
                Csequence(Cop(Ccheckbound, 
                              [float_array_length(header arr); idx]),
                          unboxed_float_array_ref arr idx))))
      end
  | Uprim(Parraysets kind, [arg1; arg2; arg3]) ->
      return_unit(begin match kind with
        Pgenarray ->
          bind "newval" (transl arg3) (fun newval ->
            bind "index" (transl arg2) (fun idx ->
              bind "arr" (transl arg1) (fun arr ->
                bind "header" (header arr) (fun hdr ->
                  Cifthenelse(is_addr_array hdr,
                    Csequence(Cop(Ccheckbound, [addr_array_length hdr; idx]),
                              addr_array_set arr idx newval),
                    Csequence(Cop(Ccheckbound, [float_array_length hdr; idx]),
                              float_array_set arr idx
                                              (unbox_float newval)))))))
      | Paddrarray ->
          bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              Csequence(Cop(Ccheckbound, [addr_array_length(header arr); idx]),
                        addr_array_set arr idx (transl arg3))))
      | Pintarray ->
          bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              Csequence(Cop(Ccheckbound, [addr_array_length(header arr); idx]),
                        int_array_set arr idx (transl arg3))))
      | Pfloatarray ->
          bind "index" (transl arg2) (fun idx ->
            bind "arr" (transl arg1) (fun arr ->
              Csequence(Cop(Ccheckbound, [float_array_length(header arr);idx]),
                        float_array_set arr idx (transl_unbox_float arg3))))
      end)

  (* Compaction of sparse switches *)
  | Uprim(Ptranslate tbl, [arg]) ->
      bind "transl" (transl arg) (fun arg ->
        let rec transl_tests lo hi =
          if lo > hi then int_const 0 else begin
            let i = (lo + hi) / 2 in
            let (first_val, last_val, ofs) = tbl.(i) in
            Cifthenelse(
              Cop(Ccmpi Clt, [arg; int_const first_val]),
              transl_tests lo (i-1),
              Cifthenelse(
                Cop(Ccmpi Cgt, [arg; int_const last_val]),
                transl_tests (i+1) hi,
                add_const arg ((ofs - first_val) * 2)))
          end in
        transl_tests 0 (Array.length tbl - 1))

  | Uprim(_, _) ->
      fatal_error "Cmmgen.transl"

  | Uswitch(arg, const_index, const_cases, block_index, block_cases) ->
      if Array.length block_index = 0 then
        transl_switch (untag_int (transl arg)) const_index const_cases
      else if Array.length const_index = 0 then
        transl_switch (get_tag (transl arg)) block_index block_cases
      else
        bind "switch" (transl arg) (fun arg ->
          Cifthenelse(
            Cop(Cand, [arg; Cconst_int 1]),
            transl_switch (untag_int arg) const_index const_cases,
            transl_switch (get_tag arg) block_index block_cases))
  | Ustaticfail ->
      Cexit
  | Ucatch(body, handler) ->
      Ccatch(transl body, transl handler)
  | Utrywith(body, exn, handler) ->
      Ctrywith(transl body, exn, transl handler)
  | Uifthenelse(Uprim(Pnot, [arg]), ifso, ifnot) ->
      transl (Uifthenelse(arg, ifnot, ifso))
  | Uifthenelse(cond, ifso, Ustaticfail) ->
      exit_if_false cond (transl ifso)
  | Uifthenelse(cond, Ustaticfail, ifnot) ->
      exit_if_true cond (transl ifnot)
  | Uifthenelse(Uprim(Psequand, _) as cond, ifso, ifnot) ->
      Ccatch(exit_if_false cond (transl ifso), transl ifnot)
  | Uifthenelse(Uprim(Psequor, _) as cond, ifso, ifnot) ->
      Ccatch(exit_if_true cond (transl ifnot), transl ifso)
  | Uifthenelse(cond, ifso, ifnot) ->
      Cifthenelse(test_bool(transl cond), transl ifso, transl ifnot)
  | Usequence(exp1, exp2) ->
      Csequence(remove_unit(transl exp1), transl exp2)
  | Uwhile(cond, body) ->
      return_unit(Ccatch(Cloop(exit_if_false cond (remove_unit(transl body))),
                         Ctuple []))
  | Ufor(id, low, high, dir, body) ->
      let tst = match dir with Upto -> Cgt   | Downto -> Clt in
      let inc = match dir with Upto -> Caddi | Downto -> Csubi in
      return_unit
        (Clet(id, transl low,
          bind "bound" (transl high) (fun high ->
            Ccatch(
              Cloop(Cifthenelse(
                Cop(Ccmpi tst, [Cvar id; high]),
                Cexit,
                 Csequence(remove_unit(transl body),
                           Cassign(id, Cop(inc, 
                                           [Cvar id; Cconst_int 2]))))),
              Ctuple []))))

and transl_unbox_float = function
    Uconst(Const_base(Const_float f)) -> Cconst_float f
  | exp -> unbox_float(transl exp)

and exit_if_true cond otherwise =
  match cond with
    Uprim(Psequor, [arg1; arg2]) ->
      exit_if_true arg1 (exit_if_true arg2 otherwise)
  | Uprim(Psequand, [arg1; arg2]) ->
      Csequence(Ccatch(exit_if_true arg1 (Ctuple []),
                       exit_if_true arg2 (Ctuple [])),
                otherwise)
  | _ ->
      Cifthenelse(test_bool(transl cond), Cexit, otherwise)

and exit_if_false cond otherwise =
  match cond with
    Uprim(Psequand, [arg1; arg2]) ->
      exit_if_false arg1 (exit_if_false arg2 otherwise)
  | Uprim(Psequor, [arg1; arg2]) ->
      Csequence(Ccatch(exit_if_false arg1 (Ctuple []),
                       exit_if_false arg2 (Ctuple [])),
                otherwise)
  | _ ->
      Cifthenelse(test_bool(transl cond), otherwise, Cexit)

and transl_switch arg index cases =
  match Array.length index with
    1 -> transl cases.(0)
  | 2 -> Cifthenelse(arg, transl cases.(index.(1)), transl cases.(index.(0)))
  | _ -> Cswitch(arg, index, Array.map transl cases)

and transl_letrec bindings cont =
  let rec init_blocks = function
      [] -> fill_blocks bindings
    | (id, exp) :: rem ->
        Clet(id, dummy_block(expr_size exp), init_blocks rem)
  and fill_blocks = function
      [] -> cont
    | (id, exp) :: rem ->
        Csequence(store_contents (Cvar id) (transl exp), fill_blocks rem)
  in init_blocks bindings

(* Translate a function definition *)

let transl_function lbl params body =
  Cfunction {fun_name = lbl;
             fun_args = List.map (fun id -> (id, typ_addr)) params;
             fun_body = transl body;
             fun_fast = !Clflags.optimize_for_speed}

(* Translate all function definitions *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let rec transl_all_functions already_translated cont =
  try
    let (lbl, params, body) = Queue.take functions in
    if StringSet.mem lbl already_translated then
      transl_all_functions already_translated cont
    else
      transl_all_functions (StringSet.add lbl already_translated)
                           (transl_function lbl params body :: cont)
  with Queue.Empty ->
    cont

(* Translate a toplevel structure definition *)

let rec transl_structure glob = function
    Uprim(Pmakeblock tag, args) ->
      (* Scan the args, storing those that are not identifiers and
         returning a map id -> position in block for those that are idents. *)
      let rec make_stores pos map = function
        [] -> (Ctuple [], map)
      | Uvar v :: rem ->
          make_stores (pos+1) (Tbl.add v pos map) rem
      | ulam :: rem ->
          let (c, final_map) = make_stores (pos+1) map rem in
          (Csequence(Cop(Cstore, [field_address (Cconst_symbol glob) pos;
                                  transl ulam]), c),
           final_map) in
      let (c, map) = make_stores 0 Tbl.empty args in
      (c, map, List.length args)
  | Usequence(e1, e2) ->
      let (c2, map, size) = transl_structure glob e2 in
      (Csequence(remove_unit(transl e1), c2), map, size)
  | Ulet(id, arg, body) ->
      let (cbody, map, size) = transl_structure glob body in
      (Clet(id, transl arg, add_store glob id map cbody), map, size)
  | Uletrec(bindings, body) ->
      let (cbody, map, size) = transl_structure glob body in
      (transl_letrec bindings (add_stores glob bindings map cbody), map, size)
  | Uprim(Psetglobal id, [arg]) ->
      transl_structure glob arg
  | _ ->
      fatal_error "Cmmgen.transl_structure"

and add_store glob id map code =
  try
    let pos = Tbl.find id map in
    Csequence(Cop(Cstore, [field_address (Cconst_symbol glob) pos; Cvar id]),
              code)
  with Not_found ->
    code

and add_stores glob bindings map code =
  match bindings with
    [] -> code
  | (id, def) :: rem -> add_stores glob rem map (add_store glob id map code) 

(* Emit structured constants *)

let rec emit_constant symb cst cont =
  match cst with
    Const_base(Const_float s) ->
      Cint(float_header) :: Cdefine_symbol symb :: Cfloat s :: cont
  | Const_base(Const_string s) ->
      Cint(string_header (String.length s)) ::
      Cdefine_symbol symb ::
      emit_string_constant s cont
  | Const_block(tag, fields) ->
      let (emit_fields, cont1) = emit_constant_fields fields cont in
      Cint(block_header tag (List.length fields)) ::
      Cdefine_symbol symb ::
      emit_fields @ cont1
  | Const_float_array(fields) ->
      Cint(floatarray_header (List.length fields)) ::
      Cdefine_symbol symb ::
      Misc.map_end (fun f -> Cfloat f) fields cont
  | _ -> fatal_error "gencmm.emit_constant"

and emit_constant_fields fields cont =
  match fields with
    [] -> ([], cont)
  | f1 :: fl ->
      let (data1, cont1) = emit_constant_field f1 cont in
      let (datal, contl) = emit_constant_fields fl cont1 in
      (data1 :: datal, contl)

and emit_constant_field field cont =
  match field with
    Const_base(Const_int n) ->
      (Cint((n lsl 1) + 1), cont)
  | Const_base(Const_char c) ->
      (Cint(((Char.code c) lsl 1) + 1), cont)
  | Const_base(Const_float s) ->
      let lbl = new_const_label() in
      (Clabel_address lbl,
       Cint(float_header) :: Cdefine_label lbl :: Cfloat s :: cont)
  | Const_base(Const_string s) ->
      let lbl = new_const_label() in
      (Clabel_address lbl,
       Cint(string_header (String.length s)) :: Cdefine_label lbl :: 
       emit_string_constant s cont)
  | Const_pointer n ->
      (Cint((n lsl 1) + 1), cont)
  | Const_block(tag, fields) ->
      let lbl = new_const_label() in
      let (emit_fields, cont1) = emit_constant_fields fields cont in
      (Clabel_address lbl,
       Cint(block_header tag (List.length fields)) :: Cdefine_label lbl ::
       emit_fields @ cont1)
  | Const_float_array(fields) ->
      let lbl = new_const_label() in
      (Clabel_address lbl,
       Cint(floatarray_header (List.length fields)) :: Cdefine_label lbl ::
       Misc.map_end (fun f -> Cfloat f) fields cont)

and emit_string_constant s cont =
  let n = size_int - 1 - (String.length s) mod size_int in
  Cstring s :: Cskip n :: Cint8 n :: cont

(* Emit all structured constants *)

let rec emit_all_constants cont =
  match !structured_constants with
    [] -> cont
  | (lbl, cst) :: rem ->
      structured_constants := rem;
      emit_all_constants (Cdata(emit_constant lbl cst []) :: cont)

(* Translate a compilation unit *)

let compunit ulam =
  let glob = Compilenv.current_unit_name () in
  let (init_code, _, compunit_size) = transl_structure glob ulam in
  let c1 = [Cfunction {fun_name = glob ^ "_entry"; fun_args = [];
                       fun_body = init_code; fun_fast = false}] in
  let c2 = transl_all_functions StringSet.empty c1 in
  let c3 = emit_all_constants c2 in
  Cdata [Cint(block_header 0 compunit_size);
         Cdefine_symbol glob;
         Cskip(compunit_size * size_addr)] :: c3

(* Generate an application function:
     (defun caml_applyN (a1 ... aN clos)
       (if (= clos.arity N)
         (app clos.direct a1 ... aN clos)
         (let (clos1 (app clos.code a1 clos)
               clos2 (app clos1.code a2 clos)
               ...
               closN-1 (app closN-2.code aN-1 closN-2))
           (app closN-1.code aN closN-1))))
*)

let apply_function arity =
  let arg = Array.new arity (Ident.new "arg") in
  for i = 1 to arity - 1 do arg.(i) <- Ident.new "arg" done;
  let clos = Ident.new "clos" in
  let rec app_fun clos n =
    if n = arity-1 then
      Cop(Capply typ_addr,
          [get_field (Cvar clos) 0; Cvar arg.(n); Cvar clos])
    else begin
      let newclos = Ident.new "clos" in
      Clet(newclos,
           Cop(Capply typ_addr,
               [get_field (Cvar clos) 0; Cvar arg.(n); Cvar clos]),
           app_fun newclos (n+1))
    end in
  let all_args = Array.to_list arg @ [clos] in
  let body =
    Cifthenelse(
      Cop(Ccmpi Ceq, [get_field (Cvar clos) 1; int_const arity]),
      Cop(Capply typ_addr,
          get_field (Cvar clos) 2 :: List.map (fun s -> Cvar s) all_args),
      app_fun clos 0) in
  Cfunction
   {fun_name = "caml_apply" ^ string_of_int arity;
    fun_args = List.map (fun id -> (id, typ_addr)) all_args;
    fun_body = body;
    fun_fast = true}

(* Generate currying functions:
      (defun caml_curryN (arg clos)
         (alloc HDR caml_curryN_1 arg clos))
      (defun caml_curryN_1 (arg clos)
         (alloc HDR caml_curryN_2 arg clos))
      ...
      (defun caml_curryN_N-1 (arg clos)
         (let (closN-2 clos.cdr
               closN-3 closN-2.cdr
               ...
               clos1 clos2.cdr
               clos clos1.cdr)
           (app clos.direct
                clos1.car clos2.car ... closN-2.car clos.car arg clos))) *)

let final_curry_function arity =
  let last_arg = Ident.new "arg" in
  let last_clos = Ident.new "clos" in
  let rec curry_fun args clos n =
    if n = 0 then
      Cop(Capply typ_addr,
          get_field (Cvar clos) 2 ::
          args @ [Cvar last_arg; Cvar clos])
    else begin
      let newclos = Ident.new "clos" in
      Clet(newclos,
           get_field (Cvar clos) 3,
           curry_fun (get_field (Cvar clos) 2 :: args) newclos (n-1))
    end in
  Cfunction
   {fun_name = "caml_curry" ^ string_of_int arity ^
               "_" ^ string_of_int (arity-1);
    fun_args = [last_arg, typ_addr; last_clos, typ_addr];
    fun_body = curry_fun [] last_clos (arity-1);
    fun_fast = true}

let rec intermediate_curry_functions arity num =
  if num = arity - 1 then
    [final_curry_function arity]
  else begin
    let name1 = "caml_curry" ^ string_of_int arity in
    let name2 = if num = 0 then name1 else name1 ^ "_" ^ string_of_int num in
    let arg = Ident.new "arg" and clos = Ident.new "clos" in
    Cfunction
     {fun_name = name2;
      fun_args = [arg, typ_addr; clos, typ_addr];
      fun_body = Cop(Calloc,
                     [alloc_closure_header 4; 
                      Cconst_symbol(name1 ^ "_" ^ string_of_int (num+1));
                      int_const 1; Cvar arg; Cvar clos]);
      fun_fast = true}
    :: intermediate_curry_functions arity (num+1)
  end
    
let curry_function arity =
  intermediate_curry_functions arity 0

(* Generate the entry point *)

let entry_point namelist =
  let body =
    List.fold_right
      (fun name next ->
        Csequence(Cop(Capply typ_void, [Cconst_symbol(name ^ "_entry")]),
                  next))
      namelist (Ctuple []) in
  Cfunction {fun_name = "caml_program";
             fun_args = [];
             fun_body = body;
             fun_fast = false}

(* Generate the table of globals and the master table of frame descriptors *)

let global_table namelist =
  Cdata(Cdefine_symbol "caml_globals" ::
        List.map (fun name -> Csymbol_address name) namelist @
        [Cint 0])

let frame_table namelist =
  Cdata(Cdefine_symbol "caml_frametable" ::
        List.map (fun name -> Csymbol_address(name ^ "_frametable")) namelist @
        [Cint 0])

(* Initialize a predefined exception *)

let predef_exception name =
  Cdata(emit_constant name (Const_block(0,[Const_base(Const_string name)])) [])
