(* Translation from closed lambda to C-- *)

open Misc
open Arch
open Asttypes
open Lambda
open Clambda
open Cmm

(* Block headers. Meaning of the tag field:
       0xFF: infix header
       0xFE: finalized
       0xFD: abstract
       0xFC: string
       0xFB: float
       0xFA: closure
       0 - 0xF9: regular blocks *)

let block_header tag sz = (sz lsl 11) + tag
let closure_header sz = block_header 0xFA sz
let infix_header ofs = block_header 0xFF ofs
let float_header = block_header 0xFB (size_float / size_addr)
let string_header len = block_header 0xFC ((len + size_addr) / size_addr)

let modified = 1 lsl 10
let alloc_block_header tag sz = Cconst_int((block_header tag sz) lor modified)
let alloc_closure_header sz = Cconst_int((closure_header sz) lor modified)
let alloc_infix_header ofs = Cconst_int(infix_header ofs)

(* Integers *)

let int_const n = Cconst_int((n lsl 1 + 1))

let add_const c n =
  if n = 0 then c else Cop(Caddi, [c; Cconst_int n])

let incr_int = function
    Cop(Caddi, [c; Cconst_int n]) -> add_const c (n+1)
  | c -> add_const c 1

let decr_int = function
    Cop(Caddi, [c; Cconst_int n]) -> add_const c (n-1)
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

let box_float c = Cop(Calloc, [Cconst_int float_header; c])

let unbox_float = function
    Cop(Calloc, [header; c]) -> c
  | c -> Cop(Cload typ_float, [c])

(* Unit *)

let return_unit c = Csequence(c, Cconst_int 1)

let rec remove_unit = function
    Csequence(c, Cconst_int 1) -> c
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

let tag_offset =
  if big_endian then -1 else -size_addr

let get_tag ptr =
  Cop(Cloadchunk Byte_unsigned,
      [Cop(Cadda, [ptr; Cconst_int(tag_offset)])])

(* Determine if a clambda is guaranteed to return an integer or a pointer
   outside the heap, making it unneccesary to do Cmodify. *)

let rec is_outside_heap = function
    Uconst _ -> true
  | Uprim(p, _) ->
      begin match p with
          Pnot | Pnegint | Paddint | Psubint | Pmulint | Pdivint | Pmodint
        | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
        | Pintcomp _ | Poffsetint _ | Pfloatcomp _
        | Pgetstringchar | Pvectlength -> true
        | _ -> false
      end
  | _ -> false

(* Array indexing *)

let log2_size_addr = Misc.log2 size_addr

let lsl_const c n =
  Cop(Clsl, [c; Cconst_int n])

let array_indexing ptr ofs =
  match ofs with
    Cconst_int n ->
      field_address ptr (n asr 1)
  | Cop(Caddi, [Cop(Clsl, [c; Cconst_int 1]); Cconst_int 1]) ->
      Cop(Cadda, [ptr; lsl_const c log2_size_addr])
  | Cop(Caddi, [c; Cconst_int n]) ->
      Cop(Cadda, [ptr; add_const (lsl_const c (log2_size_addr - 1))
                                 ((n - 1) lsl (log2_size_addr - 1))])
  | _ ->
      Cop(Cadda, [ptr; add_const (lsl_const ofs (log2_size_addr - 1))
                                 ((-1) lsl (log2_size_addr - 1))])

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

(* Local binding of complex expressions *)

let bind name arg fn =
  match arg with
    Cvar id -> fn id
  | _ -> let id = Ident.new name in Clet(id, arg, fn id)

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
      bind "fun" (transl clos) (fun clos_var ->
        Cop(Capply typ_addr,
            [get_field (Cvar clos_var) 0; transl arg; Cvar clos_var]))
  | Ugeneric_apply(clos, args) ->
      let arity = List.length args in
      Cop(Capply typ_addr,
          Cconst_symbol(apply_function arity) ::
          List.map transl (args @ [clos]))
  | Ulet(id, exp, body) ->
      Clet(id, transl exp, transl body)
  | Uletrec(bindings, body) ->
      let rec init_blocks = function
          [] -> fill_blocks bindings
        | (id, exp) :: rem ->
            Clet(id, dummy_block(expr_size exp), init_blocks rem)
      and fill_blocks = function
          [] -> transl body
        | (id, exp) :: rem ->
            Csequence(store_contents (Cvar id) (transl exp),
                      fill_blocks rem)
      in init_blocks bindings
  | Uprim(Pidentity, [arg]) ->
      transl arg
  | Uprim(Pgetglobal id, []) ->
      Cop(Cload typ_addr, [Cconst_symbol(Ident.name id)])
  | Uprim(Psetglobal id, [arg]) ->
      Cop(Cstore, [Cconst_symbol(Ident.name id); transl arg])
  | Uprim(Pmakeblock tag, []) ->
      transl_constant(Const_block(tag, []))
  | Uprim(Pmakeblock tag, args) ->
      Cop(Calloc, alloc_block_header tag (List.length args) ::
                  List.map transl args)
  | Uprim(Pfield n, [arg]) ->
      get_field (transl arg) n
  | Uprim(Psetfield n, [loc; newval]) ->
      let c =
        if is_outside_heap newval then
          set_field (transl loc) n (transl newval)
        else
          bind "modify" (transl loc) (fun loc_var ->
            Csequence(Cop(Cmodify, [Cvar loc_var]),
                      set_field (transl loc) n (transl newval)))
      in return_unit c
  | Uprim(Pccall(lbl, arity), args) ->
      Cop(Cextcall(lbl, typ_addr), List.map transl args)
  | Uprim(Praise, [arg]) ->
      Cop(Craise, [transl arg])
  | Uprim(Psequand, [arg1; arg2]) ->
      Cifthenelse(test_bool(transl arg1), transl arg2, Cconst_int 1)
  | Uprim(Psequor, [arg1; arg2]) ->
      Cifthenelse(test_bool(transl arg1), Cconst_int 3, transl arg2)
  | Uprim(Pnot, [arg]) ->
      Cop(Csubi, [Cconst_int 4; transl arg]) (* 1 -> 3, 3 -> 1 *)
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
      incr_int(Cop(Clsr, [decr_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Pasrint, [arg1; arg2]) ->
      incr_int(Cop(Casr, [decr_int(transl arg1); untag_int(transl arg2)]))
  | Uprim(Pintcomp cmp, [arg1; arg2]) ->
      tag_int(Cop(Ccmpi(transl_comparison cmp), [transl arg1; transl arg2]))
  | Uprim(Poffsetint n, [arg]) ->
      add_const (transl arg) (n lsl 1)
  | Uprim(Poffsetref n, [arg]) ->
      return_unit
        (bind "ref" (transl arg) (fun arg_var ->
          Cop(Cstore,
              [Cvar arg_var;
               add_const (Cop(Cload typ_int, [Cvar arg_var])) (n lsl 1)])))
  | Uprim(Pnegfloat, [arg]) ->
      box_float(Cop(Caddf, [Cconst_float "0.0";
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
      Cifthenelse(Cop(Ccmpf(transl_comparison cmp),
                      [transl_unbox_float arg1; transl_unbox_float arg2]),
                  int_const 1, int_const 0)
  | Uprim(Pgetstringchar, [arg1; arg2]) ->
      tag_int(Cop(Cloadchunk Byte_unsigned,
                  [add_int (transl arg1) (untag_int(transl arg2))]))
  | Uprim(Psetstringchar, [arg1; arg2; arg3]) ->
      return_unit(Cop(Cstorechunk Byte_unsigned,
                      [add_int (transl arg1) (untag_int(transl arg2));
                       transl arg3]))
  | Uprim(Pvectlength, [arg]) ->
      Cop(Cor, [Cop(Clsr, [get_field (transl arg) (-1); Cconst_int 10]);
                Cconst_int 1])
  | Uprim(Pgetvectitem, [arg1; arg2]) ->
      Cop(Cload typ_addr, [array_indexing (transl arg1) (transl arg2)])
  | Uprim(Psetvectitem, [arg1; arg2; arg3]) ->
      let c =
        if is_outside_heap arg3 then
          Cop(Cstore, [array_indexing (transl arg1) (transl arg2);
                       transl arg3])
        else
          bind "modify" (transl arg1) (fun loc_var ->
            Csequence(Cop(Cmodify, [Cvar loc_var]),
                      Cop(Cstore,
                            [array_indexing (Cvar loc_var) (transl arg2);
                             transl arg3])))
      in return_unit c
  | Uprim(Ptranslate tbl, [arg]) ->
      bind "transl" (transl arg) (fun arg_id ->
        let rec transl_tests lo hi =
          if lo > hi then int_const 0 else begin
            let i = (lo + hi) / 2 in
            let (first_val, last_val, ofs) = tbl.(i) in
            Cifthenelse(
              Cop(Ccmpi Clt, [Cvar arg_id; int_const first_val]),
              transl_tests lo (i-1),
              Cifthenelse(
                Cop(Ccmpi Cgt, [Cvar arg_id; int_const last_val]),
                transl_tests (i+1) hi,
                add_const (Cvar arg_id) ((ofs - first_val) * 2)))
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
        bind "switch" (transl arg) (fun loc_arg ->
          Cifthenelse(
            Cop(Cand, [Cvar loc_arg; Cconst_int 1]),
            transl_switch (untag_int(Cvar loc_arg)) const_index const_cases,
            transl_switch (get_tag(Cvar loc_arg)) block_index block_cases))
  | Ustaticfail ->
      Cexit
  | Ucatch(body, handler) ->
      Ccatch(transl body, transl handler)
  | Utrywith(body, exn, handler) ->
      Ctrywith(transl body, exn, transl handler)
  | Uifthenelse(cond, ifso, ifnot) ->
      begin match cond with
        Uprim(Pnot, [arg]) ->
          transl (Uifthenelse(arg, ifnot, ifso))
      | Uprim(Psequand, _) ->
          Ccatch(exit_if_false cond (transl ifso), transl ifnot)
      | Uprim(Psequor, _) ->
          Ccatch(exit_if_true cond (transl ifnot), transl ifso)
      | _ ->
          Cifthenelse(test_bool(transl cond), transl ifso, transl ifnot)
      end
  | Usequence(exp1, exp2) ->
      Csequence(remove_unit(transl exp1), transl exp2)
  | Uwhile(cond, body) ->
      return_unit(Ccatch(Cloop(exit_if_true cond (transl body)), Ctuple []))
  | Ufor(id, low, high, dir, body) ->
      let tst = match dir with Upto -> Cgt   | Downto -> Clt in
      let inc = match dir with Upto -> Caddi | Downto -> Csubi in
      return_unit
        (Clet(id, transl low,
          bind "bound" (transl high) (fun var_high ->
            Ccatch(
              Cloop(Cifthenelse(
                Cop(Ccmpi tst, [Cvar id; Cvar var_high]),
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

(* Translate a function definition *)

let transl_function lbl params body =
  Cfunction {fun_name = lbl;
             fun_args = List.map (fun id -> (id, typ_addr)) params;
             fun_body = transl body;
             fun_fast = true}

(* Translate all function definitions *)

let rec transl_all_functions cont =
  try
    let (lbl, params, body) = Queue.take functions in
    transl_all_functions(transl_function lbl params body :: cont)
  with Queue.Empty ->
    cont

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
  Queue.clear functions;
  structured_constants := [];
  let c1 = [Cfunction {fun_name = glob ^ "_entry"; fun_args = [];
                       fun_body = transl ulam; fun_fast = false}] in
  let c2 = transl_all_functions c1 in
  let c3 = emit_all_constants c2 in
  Cdata [Cdefine_symbol glob; Cint 0] :: c3

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
