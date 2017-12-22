(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Path
open Asttypes

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

type immediate_or_pointer =
  | Immediate
  | Pointer

type initialization_or_assignment =
  | Assignment
  | Heap_initialization
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type primitive =
  | Pidentity
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
  | Prevapply
  | Pdirapply
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * mutable_flag * block_shape
  | Pfield of int
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int
  | Psetfloatfield of int * initialization_or_assignment
  | Pduprecord of Types.record_representation * int
  (* Force lazy values *)
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  | Pduparray of array_kind * mutable_flag
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on big arrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a big array *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool
  | Pbytes_load_64 of bool
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Inhibition of optimisation *)
  | Popaque

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval

and block_shape =
  value_kind list option

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

type structured_constant =
    Const_base of constant
  | Const_pointer of int
  | Const_block of int * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable

type meth_kind = Self | Public | Cached

type shared_code = (int * int) list

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  is_a_functor: bool;
  stub: bool;
}

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * Location.t
  | Lswitch of lambda * lambda_switch * Location.t
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * Location.t
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * Ident.t list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and lfunction =
  { kind: function_kind;
    params: Ident.t list;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc: Location.t; }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_loc : Location.t;
    ap_should_be_tailcall : bool;
    ap_inlined : inline_attribute;
    ap_specialised : specialise_attribute; }

and lambda_switch =
  { sw_numconsts: int;
    sw_consts: (int * lambda) list;
    sw_numblocks: int;
    sw_blocks: (int * lambda) list;
    sw_failaction : lambda option}

and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.summary }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo
  | Lev_module_definition of Ident.t

type program =
  { module_ident : Ident.t;
    main_module_block_size : int;
    required_globals : Ident.Set.t;
    code : lambda }

let const_unit = Const_pointer 0

let lambda_unit = Lconst const_unit

let default_function_attribute = {
  inline = Default_inline;
  specialise = Default_specialise;
  is_a_functor = false;
  stub = false;
}

let default_stub_attribute =
  { default_function_attribute with stub = true }

(* Build sharing keys *)
(*
   Those keys are later compared with Pervasives.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

let make_key e =
  let count = ref 0   (* Used for controling size *)
  and make_key = Ident.make_key_generator () in
  (* make_key is used for normalizing let-bound variables *)
  let rec tr_rec env e =
    incr count ;
    if !count > max_raw then raise Not_simple ; (* Too big ! *)
    match e with
    | Lvar id ->
      begin
        try Ident.find_same id env
        with Not_found -> e
      end
    | Lconst  (Const_base (Const_string _)) ->
        (* Mutable constants are not shared *)
        raise Not_simple
    | Lconst _ -> e
    | Lapply ap ->
        Lapply {ap with ap_func = tr_rec env ap.ap_func;
                        ap_args = tr_recs env ap.ap_args;
                        ap_loc = Location.none}
    | Llet (Alias,_k,x,ex,e) -> (* Ignore aliases -> substitute *)
        let ex = tr_rec env ex in
        tr_rec (Ident.add x ex env) e
    | Llet ((Strict | StrictOpt),_k,x,ex,Lvar v) when Ident.same v x ->
        tr_rec env ex
    | Llet (str,k,x,ex,e) ->
     (* Because of side effects, keep other lets with normalized names *)
        let ex = tr_rec env ex in
        let y = make_key x in
        Llet (str,k,y,ex,tr_rec (Ident.add x (Lvar y) env) e)
    | Lprim (p,es,_) ->
        Lprim (p,tr_recs env es, Location.none)
    | Lswitch (e,sw,loc) ->
        Lswitch (tr_rec env e,tr_sw env sw,loc)
    | Lstringswitch (e,sw,d,_) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s,e) -> s,tr_rec env e) sw,
           tr_opt env d,
          Location.none)
    | Lstaticraise (i,es) ->
        Lstaticraise (i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2) ->
        Lstaticcatch (tr_rec env e1,xs,tr_rec env e2)
    | Ltrywith (e1,x,e2) ->
        Ltrywith (tr_rec env e1,x,tr_rec env e2)
    | Lifthenelse (cond,ifso,ifnot) ->
        Lifthenelse (tr_rec env cond,tr_rec env ifso,tr_rec env ifnot)
    | Lsequence (e1,e2) ->
        Lsequence (tr_rec env e1,tr_rec env e2)
    | Lassign (x,e) ->
        Lassign (x,tr_rec env e)
    | Lsend (m,e1,e2,es,_loc) ->
        Lsend (m,tr_rec env e1,tr_rec env e2,tr_recs env es,Location.none)
    | Lifused (id,e) -> Lifused (id,tr_rec env e)
    | Lletrec _|Lfunction _
    | Lfor _ | Lwhile _
(* Beware: (PR#6412) the event argument to Levent
   may include cyclic structure of type Type.typexpr *)
    | Levent _  ->
        raise Not_simple

  and tr_recs env es = List.map (tr_rec env) es

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_consts ;
      sw_blocks = List.map (fun (i,e) -> i,tr_rec env e) sw.sw_blocks ;
      sw_failaction = tr_opt env sw.sw_failaction ; }

  and tr_opt env = function
    | None -> None
    | Some e -> Some (tr_rec env e) in

  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg fn =
  match arg with
    Lvar id -> fn id
  | _ -> let id = Ident.create "let" in Llet(strict, Pgenval, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar _ as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create "let" in
      Llet(Strict, Pgenval, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args


let iter_opt f = function
  | None -> ()
  | Some e -> f e

let iter_head_constructor f = function
    Lvar _
  | Lconst _ -> ()
  | Lapply{ap_func = fn; ap_args = args} ->
      f fn; List.iter f args
  | Lfunction{body} ->
      f body
  | Llet(_str, _k, _id, arg, body) ->
      f arg; f body
  | Lletrec(decl, body) ->
      f body;
      List.iter (fun (_id, exp) -> f exp) decl
  | Lprim(_p, args, _loc) ->
      List.iter f args
  | Lswitch(arg, sw,_) ->
      f arg;
      List.iter (fun (_key, case) -> f case) sw.sw_consts;
      List.iter (fun (_key, case) -> f case) sw.sw_blocks;
      iter_opt f sw.sw_failaction
  | Lstringswitch (arg,cases,default,_) ->
      f arg ;
      List.iter (fun (_,act) -> f act) cases ;
      iter_opt f default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, _, e2) ->
      f e1; f e2
  | Ltrywith(e1, _, e2) ->
      f e1; f e2
  | Lifthenelse(e1, e2, e3) ->
      f e1; f e2; f e3
  | Lsequence(e1, e2) ->
      f e1; f e2
  | Lwhile(e1, e2) ->
      f e1; f e2
  | Lfor(_v, e1, e2, _dir, e3) ->
      f e1; f e2; f e3
  | Lassign(_, e) ->
      f e
  | Lsend (_k, met, obj, args, _) ->
      List.iter f (met::obj::args)
  | Levent (lam, _evt) ->
      f lam
  | Lifused (_v, e) ->
      f e

let rec free_variables = function
  | Lvar id -> Ident.Set.singleton id
  | Lconst _ -> Ident.Set.empty
  | Lapply{ap_func = fn; ap_args = args} ->
      free_variables_list (free_variables fn) args
  | Lfunction{body; params} ->
      Ident.Set.diff (free_variables body)
        (Ident.Set.of_list params)
  | Llet(_str, _k, id, arg, body) ->
      Ident.Set.union
        (free_variables arg)
        (Ident.Set.remove id (free_variables body))
  | Lletrec(decl, body) ->
      let set = free_variables_list (free_variables body) (List.map snd decl) in
      Ident.Set.diff set (Ident.Set.of_list (List.map fst decl))
  | Lprim(_p, args, _loc) ->
      free_variables_list Ident.Set.empty args
  | Lswitch(arg, sw,_) ->
      let set =
        free_variables_list
          (free_variables_list (free_variables arg)
             (List.map snd sw.sw_consts))
          (List.map snd sw.sw_blocks)
      in
      begin match sw.sw_failaction with
      | None -> set
      | Some failaction -> Ident.Set.union set (free_variables failaction)
      end
  | Lstringswitch (arg,cases,default,_) ->
      let set =
        free_variables_list (free_variables arg)
          (List.map snd cases)
      in
      begin match default with
      | None -> set
      | Some default -> Ident.Set.union set (free_variables default)
      end
  | Lstaticraise (_,args) ->
      free_variables_list Ident.Set.empty args
  | Lstaticcatch(body, (_, params), handler) ->
      Ident.Set.union
        (Ident.Set.diff
           (free_variables handler)
           (Ident.Set.of_list params))
        (free_variables body)
  | Ltrywith(body, param, handler) ->
      Ident.Set.union
        (Ident.Set.remove
           param
           (free_variables handler))
        (free_variables body)
  | Lifthenelse(e1, e2, e3) ->
      Ident.Set.union
        (Ident.Set.union (free_variables e1) (free_variables e2))
        (free_variables e3)
  | Lsequence(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lwhile(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lfor(v, lo, hi, _dir, body) ->
      let set = Ident.Set.union (free_variables lo) (free_variables hi) in
      Ident.Set.union set (Ident.Set.remove v (free_variables body))
  | Lassign(id, e) ->
      Ident.Set.add id (free_variables e)
  | Lsend (_k, met, obj, args, _) ->
      free_variables_list
        (Ident.Set.union (free_variables met) (free_variables obj))
        args
  | Levent (lam, _evt) ->
      free_variables lam
  | Lifused (_v, e) ->
      (* Shouldn't v be considered a free variable ? *)
      free_variables e

and free_variables_list set exprs =
  List.fold_left (fun set expr -> Ident.Set.union (free_variables expr) set)
    set exprs

(* Check if an action has a "when" guard *)
let raise_count = ref 0

let next_raise_count () =
  incr raise_count ;
  !raise_count

(* Anticipated staticraise, for guards *)
let staticfail = Lstaticraise (0,[])

let rec is_guarded = function
  | Lifthenelse(_cond, _body, Lstaticraise (0,[])) -> true
  | Llet(_str, _k, _id, _lam, body) -> is_guarded body
  | Levent(lam, _ev) -> is_guarded lam
  | _ -> false

let rec patch_guarded patch = function
  | Lifthenelse (cond, body, Lstaticraise (0,[])) ->
      Lifthenelse (cond, body, patch)
  | Llet(str, k, id, lam, body) ->
      Llet (str, k, id, lam, patch_guarded patch body)
  | Levent(lam, ev) ->
      Levent (patch_guarded patch lam, ev)
  | _ -> fatal_error "Lambda.patch_guarded"

(* Translate an access path *)

let rec transl_normal_path = function
    Pident id ->
      if Ident.global id
      then Lprim(Pgetglobal id, [], Location.none)
      else Lvar id
  | Pdot(p, _s, pos) ->
      Lprim(Pfield pos, [transl_normal_path p], Location.none)
  | Papply _ ->
      fatal_error "Lambda.transl_path"

(* Translation of identifiers *)

let transl_module_path ?(loc=Location.none) env path =
  transl_normal_path (Env.normalize_path (Some loc) env path)

let transl_value_path ?(loc=Location.none) env path =
  transl_normal_path (Env.normalize_path_prefix (Some loc) env path)

let transl_class_path = transl_value_path
let transl_extension_path = transl_value_path

(* compatibility alias, deprecated in the .mli *)
let transl_path = transl_value_path

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let rec subst s lam =
  let remove_list l s =
    List.fold_left (fun s id -> Ident.Map.remove id s) s l
  in
  let module M = Ident.Map in
  match lam with
  | Lvar id as l ->
      begin try Ident.Map.find id s with Not_found -> l end
  | Lconst _ as l -> l
  | Lapply ap ->
      Lapply{ap with ap_func = subst s ap.ap_func;
                     ap_args = subst_list s ap.ap_args}
  | Lfunction{kind; params; body; attr; loc} ->
      let s = List.fold_right Ident.Map.remove params s in
      Lfunction{kind; params; body = subst s body; attr; loc}
  | Llet(str, k, id, arg, body) ->
      Llet(str, k, id, subst s arg, subst (Ident.Map.remove id s) body)
  | Lletrec(decl, body) ->
      let s =
        List.fold_left (fun s (id, _) -> Ident.Map.remove id s)
          s decl
      in
      Lletrec(List.map (subst_decl s) decl, subst s body)
  | Lprim(p, args, loc) -> Lprim(p, subst_list s args, loc)
  | Lswitch(arg, sw, loc) ->
      Lswitch(subst s arg,
              {sw with sw_consts = List.map (subst_case s) sw.sw_consts;
                       sw_blocks = List.map (subst_case s) sw.sw_blocks;
                       sw_failaction = subst_opt s sw.sw_failaction; },
              loc)
  | Lstringswitch (arg,cases,default,loc) ->
      Lstringswitch
        (subst s arg,List.map (subst_strcase s) cases,subst_opt s default,loc)
  | Lstaticraise (i,args) ->  Lstaticraise (i, subst_list s args)
  | Lstaticcatch(body, (id, params), handler) ->
      Lstaticcatch(subst s body, (id, params),
                   subst (remove_list params s) handler)
  | Ltrywith(body, exn, handler) ->
      Ltrywith(subst s body, exn, subst (Ident.Map.remove exn s) handler)
  | Lifthenelse(e1, e2, e3) -> Lifthenelse(subst s e1, subst s e2, subst s e3)
  | Lsequence(e1, e2) -> Lsequence(subst s e1, subst s e2)
  | Lwhile(e1, e2) -> Lwhile(subst s e1, subst s e2)
  | Lfor(v, lo, hi, dir, body) ->
      Lfor(v, subst s lo, subst s hi, dir,
        subst (Ident.Map.remove v s) body)
  | Lassign(id, e) ->
      assert(not (Ident.Map.mem id s));
      Lassign(id, subst s e)
  | Lsend (k, met, obj, args, loc) ->
      Lsend (k, subst s met, subst s obj, subst_list s args, loc)
  | Levent (lam, evt) -> Levent (subst s lam, evt)
  | Lifused (v, e) -> Lifused (v, subst s e)
and subst_list s l = List.map (subst s) l
and subst_decl s (id, exp) = (id, subst s exp)
and subst_case s (key, case) = (key, subst s case)
and subst_strcase s (key, case) = (key, subst s case)
and subst_opt s = function
  | None -> None
  | Some e -> Some (subst s e)


let rec map f lam =
  let lam =
    match lam with
    | Lvar _ -> lam
    | Lconst _ -> lam
    | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall;
          ap_inlined; ap_specialised } ->
        Lapply {
          ap_func = map f ap_func;
          ap_args = List.map (map f) ap_args;
          ap_loc;
          ap_should_be_tailcall;
          ap_inlined;
          ap_specialised;
        }
    | Lfunction { kind; params; body; attr; loc; } ->
        Lfunction { kind; params; body = map f body; attr; loc; }
    | Llet (str, k, v, e1, e2) ->
        Llet (str, k, v, map f e1, map f e2)
    | Lletrec (idel, e2) ->
        Lletrec (List.map (fun (v, e) -> (v, map f e)) idel, map f e2)
    | Lprim (p, el, loc) ->
        Lprim (p, List.map (map f) el, loc)
    | Lswitch (e, sw, loc) ->
        Lswitch (map f e,
          { sw_numconsts = sw.sw_numconsts;
            sw_consts = List.map (fun (n, e) -> (n, map f e)) sw.sw_consts;
            sw_numblocks = sw.sw_numblocks;
            sw_blocks = List.map (fun (n, e) -> (n, map f e)) sw.sw_blocks;
            sw_failaction = Misc.may_map (map f) sw.sw_failaction;
          },
          loc)
    | Lstringswitch (e, sw, default, loc) ->
        Lstringswitch (
          map f e,
          List.map (fun (s, e) -> (s, map f e)) sw,
          Misc.may_map (map f) default,
          loc)
    | Lstaticraise (i, args) ->
        Lstaticraise (i, List.map (map f) args)
    | Lstaticcatch (body, id, handler) ->
        Lstaticcatch (map f body, id, map f handler)
    | Ltrywith (e1, v, e2) ->
        Ltrywith (map f e1, v, map f e2)
    | Lifthenelse (e1, e2, e3) ->
        Lifthenelse (map f e1, map f e2, map f e3)
    | Lsequence (e1, e2) ->
        Lsequence (map f e1, map f e2)
    | Lwhile (e1, e2) ->
        Lwhile (map f e1, map f e2)
    | Lfor (v, e1, e2, dir, e3) ->
        Lfor (v, map f e1, map f e2, dir, map f e3)
    | Lassign (v, e) ->
        Lassign (v, map f e)
    | Lsend (k, m, o, el, loc) ->
        Lsend (k, map f m, map f o, List.map (map f) el, loc)
    | Levent (l, ev) ->
        Levent (map f l, ev)
    | Lifused (v, e) ->
        Lifused (v, map f e)
  in
  f lam

(* To let-bind expressions to variables *)

let bind str var exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, Pgenval, var, exp, body)

let negate_integer_comparison = function
  | Ceq -> Cne
  | Cne -> Ceq
  | Clt -> Cge
  | Cle -> Cgt
  | Cgt -> Cle
  | Cge -> Clt

let swap_integer_comparison = function
  | Ceq -> Ceq
  | Cne -> Cne
  | Clt -> Cgt
  | Cle -> Cge
  | Cgt -> Clt
  | Cge -> Cle

let negate_float_comparison = function
  | CFeq -> CFneq
  | CFneq -> CFeq
  | CFlt -> CFnlt
  | CFnlt -> CFlt
  | CFgt -> CFngt
  | CFngt -> CFgt
  | CFle -> CFnle
  | CFnle -> CFle
  | CFge -> CFnge
  | CFnge -> CFge

let swap_float_comparison = function
  | CFeq -> CFeq
  | CFneq -> CFneq
  | CFlt -> CFgt
  | CFnlt -> CFngt
  | CFle -> CFge
  | CFnle -> CFnge
  | CFgt -> CFlt
  | CFngt -> CFnlt
  | CFge -> CFle
  | CFnge -> CFnle

let raise_kind = function
  | Raise_regular -> "raise"
  | Raise_reraise -> "reraise"
  | Raise_notrace -> "raise_notrace"

let merge_inline_attributes attr1 attr2 =
  match attr1, attr2 with
  | Default_inline, _ -> Some attr2
  | _, Default_inline -> Some attr1
  | _, _ ->
    if attr1 = attr2 then Some attr1
    else None

let reset () =
  raise_count := 0
