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
  (* Operations on Bigarrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a Bigarray *)
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
  | Raise_regular of Location.t option
  | Raise_reraise of Location.t option
  | Raise_notrace

let equal_boxed_integer x y =
  match x, y with
  | Pnativeint, Pnativeint
  | Pint32, Pint32
  | Pint64, Pint64 ->
    true
  | (Pnativeint | Pint32 | Pint64), _ ->
    false

let equal_primitive =
  (* Should be implemented like [equal_value_kind] of [equal_boxed_integer],
     i.e. by matching over the various constructors but the type has more
     than 100 constructors... *)
  (=)

let equal_value_kind x y =
  match x, y with
  | Pgenval, Pgenval -> true
  | Pfloatval, Pfloatval -> true
  | Pboxedintval bi1, Pboxedintval bi2 -> equal_boxed_integer bi1 bi2
  | Pintval, Pintval -> true
  | (Pgenval | Pfloatval | Pboxedintval _ | Pintval), _ -> false


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

let equal_inline_attribute x y =
  match x, y with
  | Always_inline, Always_inline
  | Never_inline, Never_inline
  | Default_inline, Default_inline
    ->
    true
  | Unroll u, Unroll v ->
    u = v
  | (Always_inline | Never_inline | Unroll _ | Default_inline), _ ->
    false

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

let equal_specialise_attribute x y =
  match x, y with
  | Always_specialise, Always_specialise
  | Never_specialise, Never_specialise
  | Default_specialise, Default_specialise ->
    true
  | (Always_specialise | Never_specialise | Default_specialise), _ ->
    false

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt | Variable

type meth_kind = Self | Public | Cached

let equal_meth_kind x y =
  match x, y with
  | Self, Self -> true
  | Public, Public -> true
  | Cached, Cached -> true
  | (Self | Public | Cached), _ -> false

type shared_code = (int * int) list

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  is_a_functor: bool;
  stub: bool;
}

type lambda =
    Lvar of Ident.t
  | Lconst of structured_constant * Location.t
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lletrec of (Ident.t * lambda) list * lambda
  | Lprim of primitive * lambda list * Location.t
  | Lswitch of lambda * lambda_switch * Location.t
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of lambda * (string * block) list * block option * Location.t
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * block
  | Ltrywith of lambda * Ident.t * block
  | Lifthenelse of lambda * block * block * Location.t
  | Lsequence of lambda * lambda
  | Lwhile of lambda * block * Location.t
  | Lfor of Ident.t * lambda * lambda * direction_flag * block * Location.t
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * Location.t
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and block = {
  block_loc : Location.t;
  expr : lambda;
}

and lfunction =
  { kind: function_kind;
    params: (Ident.t * value_kind) list;
    return: value_kind;
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
    sw_consts: (int * block) list;
    sw_numblocks: int;
    sw_blocks: (int * block) list;
    sw_failaction : block option}

and lambda_event =
  { lev_loc: Location.t;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.t }

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

let lambda_unit loc = Lconst (const_unit, loc)

let default_function_attribute = {
  inline = Default_inline;
  specialise = Default_specialise;
  local = Default_local;
  is_a_functor = false;
  stub = false;
}

let default_stub_attribute =
  { default_function_attribute with stub = true }

let block block_loc expr =
  { block_loc;
    expr;
  }

(* Build sharing keys *)
(*
   Those keys are later compared with Stdlib.compare.
   For that reason, they should not include cycles.
*)

exception Not_simple

let max_raw = 32

(* This function should erase location information.  If switch branches are
   shared then the resulting location will be chosen arbitrarily from
   the locations of the shared branches. *)
let make_key e =
  let count = ref 0   (* Used for controlling size *)
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
    | Lconst  (Const_base (Const_string _), _loc) ->
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
        Lprim (p,tr_recs env es,Location.none)
    | Lswitch (e,sw,loc) ->
        Lswitch (tr_rec env e,tr_sw env sw,loc)
    | Lstringswitch (e,sw,d,loc) ->
        Lstringswitch
          (tr_rec env e,
           List.map (fun (s, act) -> s, tr_block env act) sw,
           tr_default env d,
           loc)
    | Lstaticraise (i,es) ->
        Lstaticraise (i,tr_recs env es)
    | Lstaticcatch (e1,xs,e2) ->
        Lstaticcatch (tr_rec env e1,xs, tr_block env e2)
    | Ltrywith (e1,x,e2) ->
        Ltrywith (tr_rec env e1,x, tr_block env e2)
    | Lifthenelse (cond,ifso,ifnot,loc) ->
        Lifthenelse (tr_rec env cond, tr_block env ifso, tr_block env ifnot,
          loc)
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

  and tr_switch_arm env (key, act) =
    key, tr_block env act

  and tr_sw env sw =
    { sw with
      sw_consts = List.map (fun act -> tr_switch_arm env act) sw.sw_consts;
      sw_blocks = List.map (fun act -> tr_switch_arm env act) sw.sw_blocks;
      sw_failaction = tr_default env sw.sw_failaction;
    }

  and tr_default env = Option.map (tr_block env)

  and tr_block env { block_loc = _; expr; } =
    { block_loc = Location.none;
      expr = tr_rec env expr;
    }
  in
  try
    Some (tr_rec Ident.empty e)
  with Not_simple -> None

(***************)

let name_lambda strict arg fn =
  match arg with
    Lvar id -> fn id
  | _ ->
      let id = Ident.create_local "let" in
      Llet(strict, Pgenval, id, arg, fn id)

let name_lambda_list args fn =
  let rec name_list names = function
    [] -> fn (List.rev names)
  | (Lvar _ as arg) :: rem ->
      name_list (arg :: names) rem
  | arg :: rem ->
      let id = Ident.create_local "let" in
      Llet(Strict, Pgenval, id, arg, name_list (Lvar id :: names) rem) in
  name_list [] args

let shallow_iter ~tail ~non_tail:f = function
    Lvar _
  | Lconst _ -> ()
  | Lapply{ap_func = fn; ap_args = args} ->
      f fn; List.iter f args
  | Lfunction{body} ->
      f body
  | Llet(_str, _k, _id, arg, body) ->
      f arg; tail body
  | Lletrec(decl, body) ->
      tail body;
      List.iter (fun (_id, exp) -> f exp) decl
  | Lprim (Pidentity, [l], _) ->
      tail l
  | Lprim (Psequand, [l1; l2], _)
  | Lprim (Psequor, [l1; l2], _) ->
      f l1;
      tail l2
  | Lprim(_p, args, _loc) ->
      List.iter f args
  | Lswitch (arg, sw, _loc) ->
      f arg;
      List.iter (fun (_key, act) -> tail act.expr) sw.sw_consts;
      List.iter (fun (_key, act) -> tail act.expr) sw.sw_blocks;
      Option.iter (fun act -> tail act.expr) sw.sw_failaction
  | Lstringswitch (arg, cases, default, _loc) ->
      f arg;
      List.iter (fun (_key, act) -> tail act.expr) cases;
      Option.iter (fun act -> tail act.expr) default
  | Lstaticraise (_,args) ->
      List.iter f args
  | Lstaticcatch(e1, _, e2) ->
      tail e1; tail e2.expr
  | Ltrywith(e1, _, e2) ->
      f e1; tail e2.expr
  | Lifthenelse(e1, e2, e3, _loc) ->
      f e1; tail e2.expr; tail e3.expr
  | Lsequence(e1, e2) ->
      f e1; tail e2
  | Lwhile(e1, e2, _loc) ->
      f e1; f e2.expr
  | Lfor(_v, e1, e2, _dir, e3, _loc) ->
      f e1; f e2; f e3.expr
  | Lassign(_, e) ->
      f e
  | Lsend (_k, met, obj, args, _) ->
      List.iter f (met::obj::args)
  | Levent (e, _evt) ->
      tail e
  | Lifused (_v, e) ->
      tail e

let iter_head_constructor f l =
  shallow_iter ~tail:f ~non_tail:f l

let rec free_variables = function
  | Lvar id -> Ident.Set.singleton id
  | Lconst _ -> Ident.Set.empty
  | Lapply{ap_func = fn; ap_args = args} ->
      free_variables_list (free_variables fn) args
  | Lfunction{body; params} ->
      Ident.Set.diff (free_variables body)
        (Ident.Set.of_list (List.map fst params))
  | Llet(_str, _k, id, arg, body) ->
      Ident.Set.union
        (free_variables arg)
        (Ident.Set.remove id (free_variables body))
  | Lletrec(decl, body) ->
      let set = free_variables_list (free_variables body) (List.map snd decl) in
      Ident.Set.diff set (Ident.Set.of_list (List.map fst decl))
  | Lprim(_p, args, _loc) ->
      free_variables_list Ident.Set.empty args
  | Lswitch(arg, sw, _loc) ->
      let set =
        free_variables_list
          (free_variables_list (free_variables arg)
             (List.map (fun (_, const) -> const.expr) sw.sw_consts))
          (List.map (fun (_, block) -> block.expr) sw.sw_blocks)
      in
      begin match sw.sw_failaction with
      | None -> set
      | Some failaction ->
        Ident.Set.union set (free_variables failaction.expr)
      end
  | Lstringswitch (arg, cases, default, _loc) ->
      let set =
        free_variables_list (free_variables arg)
          (List.map (fun (_, str) -> str.expr) cases)
      in
      begin match default with
      | None -> set
      | Some default -> Ident.Set.union set (free_variables default.expr)
      end
  | Lstaticraise (_,args) ->
      free_variables_list Ident.Set.empty args
  | Lstaticcatch(body, (_, params), handler) ->
      Ident.Set.union
        (Ident.Set.diff
           (free_variables handler.expr)
           (Ident.Set.of_list (List.map fst params)))
        (free_variables body)
  | Ltrywith(body, param, handler) ->
      Ident.Set.union
        (Ident.Set.remove
           param
           (free_variables handler.expr))
        (free_variables body)
  | Lifthenelse(e1, e2, e3, _loc) ->
      Ident.Set.union
        (Ident.Set.union (free_variables e1) (free_variables e2.expr))
        (free_variables e3.expr)
  | Lsequence(e1, e2) ->
      Ident.Set.union (free_variables e1) (free_variables e2)
  | Lwhile(e1, e2, _loc) ->
      Ident.Set.union (free_variables e1) (free_variables e2.expr)
  | Lfor(v, lo, hi, _dir, body, _loc) ->
      let set = Ident.Set.union (free_variables lo) (free_variables hi) in
      Ident.Set.union set (Ident.Set.remove v (free_variables body.expr))
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

(* Translate an access path *)

let rec transl_address loc = function
  | Env.Aident id ->
      if Ident.global id
      then Lprim(Pgetglobal id, [], loc)
      else Lvar id
  | Env.Adot(addr, pos) ->
      Lprim(Pfield pos, [transl_address loc addr], loc)

let transl_path find loc env path =
  match find path env with
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))
  | addr -> transl_address loc addr

(* Translation of identifiers *)

let transl_module_path loc env path =
  transl_path Env.find_module_address loc env path

let transl_value_path loc env path =
  transl_path Env.find_value_address loc env path

let transl_extension_path loc env path =
  transl_path Env.find_constructor_address loc env path

let transl_class_path loc env path =
  transl_path Env.find_class_address loc env path

let transl_prim loc mod_name name =
  let pers = Ident.create_persistent mod_name in
  let env = Env.add_persistent_structure pers Env.empty in
  let lid = Longident.Ldot (Longident.Lident mod_name, name) in
  match Env.lookup_value lid env with
  | path, _ -> transl_value_path loc env path
  | exception Not_found ->
      fatal_error ("Primitive " ^ name ^ " not found.")

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit Location.none
  | [x] -> fn x
  | x::rem ->
      let lam = fn x in Lsequence(lam, make_sequence fn rem)

(* Apply a substitution to a lambda-term.
   Assumes that the image of the substitution is out of reach
   of the bound variables of the lambda-term (no capture). *)

let subst update_env s lam =
  let rec subst s lam =
    let remove_list l s =
      List.fold_left (fun s (id, _kind) -> Ident.Map.remove id s) s l
    in
    let module M = Ident.Map in
    match lam with
    | Lvar id as l ->
        begin try Ident.Map.find id s with Not_found -> l end
    | Lconst _ as l -> l
    | Lapply ap ->
        Lapply{ap with ap_func = subst s ap.ap_func;
                      ap_args = subst_list s ap.ap_args}
    | Lfunction lf ->
        let s =
          List.fold_right
            (fun (id, _) s -> Ident.Map.remove id s)
            lf.params s
        in
        Lfunction {lf with body = subst s lf.body}
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
                        sw_failaction = subst_default s sw.sw_failaction; },
                loc)
    | Lstringswitch (arg, cases, default, loc) ->
        Lstringswitch
          (subst s arg,List.map (subst_strcase s) cases,
            subst_default s default, loc)
    | Lstaticraise (i,args) ->  Lstaticraise (i, subst_list s args)
    | Lstaticcatch(body, (id, params), handler) ->
        Lstaticcatch(subst s body, (id, params),
                    subst_block (remove_list params s) handler)
    | Ltrywith(body, exn, handler) ->
        Ltrywith(subst s body, exn,
          subst_block (Ident.Map.remove exn s) handler)
    | Lifthenelse(e1, e2, e3, loc) ->
        Lifthenelse(subst s e1, subst_block s e2, subst_block s e3, loc)
    | Lsequence(e1, e2) -> Lsequence(subst s e1, subst s e2)
    | Lwhile(e1, e2, loc) -> Lwhile(subst s e1, subst_block s e2, loc)
    | Lfor(v, lo, hi, dir, body, loc) ->
        Lfor(v, subst s lo, subst s hi, dir,
          subst_block (Ident.Map.remove v s) body, loc)
    | Lassign(id, e) ->
        assert(not (Ident.Map.mem id s));
        Lassign(id, subst s e)
    | Lsend (k, met, obj, args, loc) ->
        Lsend (k, subst s met, subst s obj, subst_list s args, loc)
    | Levent (lam, evt) ->
        let lev_env =
          Ident.Map.fold (fun id _ env ->
            match Env.find_value (Path.Pident id) evt.lev_env with
            | exception Not_found -> env
            | vd -> update_env id vd env
          ) s evt.lev_env
        in
        Levent (subst s lam, { evt with lev_env })
    | Lifused (v, e) -> Lifused (v, subst s e)
  and subst_block s { block_loc; expr; } =
    { block_loc;
      expr = subst s expr;
    }
  and subst_list s l = List.map (subst s) l
  and subst_decl s (id, exp) = (id, subst s exp)
  and subst_case s (key, act) = (key, subst_block s act)
  and subst_strcase s (key, act) = (key, subst_block s act)
  and subst_default s default =
    Option.map (fun act -> subst_block s act) default
  in
  subst s lam

let rename idmap lam =
  let update_env oldid vd env =
    let newid = Ident.Map.find oldid idmap in
    Env.add_value newid vd env
  in
  let s = Ident.Map.map (fun new_id -> Lvar new_id) idmap in
  subst update_env s lam

let map_block f { block_loc; expr; } =
  { block_loc;
    expr = f expr;
  }

let shallow_map f = function
  | Lvar _
  | Lconst _ as lam -> lam
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall;
             ap_inlined; ap_specialised } ->
      Lapply {
        ap_func = f ap_func;
        ap_args = List.map f ap_args;
        ap_loc;
        ap_should_be_tailcall;
        ap_inlined;
        ap_specialised;
      }
  | Lfunction { kind; params; return; body; attr; loc; } ->
      Lfunction { kind; params; return; body = f body; attr; loc; }
  | Llet (str, k, v, e1, e2) ->
      Llet (str, k, v, f e1, f e2)
  | Lletrec (idel, e2) ->
      Lletrec (List.map (fun (v, e) -> (v, f e)) idel, f e2)
  | Lprim (p, el, loc) ->
      Lprim (p, List.map f el, loc)
  | Lswitch (e, sw, loc) ->
      Lswitch (f e,
               { sw_numconsts = sw.sw_numconsts;
                 sw_consts =
                   List.map (fun (n, e) -> (n, map_block f e)) sw.sw_consts;
                 sw_numblocks = sw.sw_numblocks;
                 sw_blocks =
                   List.map (fun (n, e) -> (n, map_block f e)) sw.sw_blocks;
                 sw_failaction = Option.map (map_block f) sw.sw_failaction;
               },
               loc)
  | Lstringswitch (e, sw, default, loc) ->
      Lstringswitch (
        f e,
        List.map (fun (s, e) -> (s, map_block f e)) sw,
        Option.map (map_block f) default,
        loc)
  | Lstaticraise (i, args) ->
      Lstaticraise (i, List.map f args)
  | Lstaticcatch (body, id, handler) ->
      Lstaticcatch (f body, id, map_block f handler)
  | Ltrywith (e1, v, e2) ->
      Ltrywith (f e1, v, map_block f e2)
  | Lifthenelse (e1, e2, e3, loc) ->
      Lifthenelse (f e1, map_block f e2, map_block f e3, loc)
  | Lsequence (e1, e2) ->
      Lsequence (f e1, f e2)
  | Lwhile (e1, e2, loc) ->
      Lwhile (f e1, map_block f e2, loc)
  | Lfor (v, e1, e2, dir, e3, loc) ->
      Lfor (v, f e1, f e2, dir, map_block f e3, loc)
  | Lassign (v, e) ->
      Lassign (v, f e)
  | Lsend (k, m, o, el, loc) ->
      Lsend (k, f m, f o, List.map f el, loc)
  | Levent (l, ev) ->
      Levent (f l, ev)
  | Lifused (v, e) ->
      Lifused (v, f e)

let map f =
  let rec g lam = f (shallow_map g lam) in
  g

(* To let-bind expressions to variables *)

let bind_with_value_kind str (var, kind) exp body =
  match exp with
    Lvar var' when Ident.same var var' -> body
  | _ -> Llet(str, kind, var, exp, body)

let bind str var exp body =
  bind_with_value_kind str (var, Pgenval) exp body

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
  | Raise_regular _loc -> "raise"
  | Raise_reraise _loc -> "reraise"
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
