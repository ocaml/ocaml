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

(*  bytegen.ml : translation of lambda terms to lists of instructions. *)

open Misc
open Asttypes
open Primitive
open Lambda
open Switch
open Instruct
open Debuginfo.Scoped_location

(**** Label generation ****)

let label_counter = ref 0

let new_label () =
  incr label_counter; !label_counter

(**** Operations on compilation environments. ****)

let empty_env =
  { ce_stack = Ident.empty; ce_closure = Not_in_closure }

(* Add a stack-allocated variable *)

let add_var id pos env =
  { ce_stack = Ident.add id pos env.ce_stack;
    ce_closure = env.ce_closure }

let rec add_vars idlist pos env =
  match idlist with
    [] -> env
  | id :: rem -> add_vars rem (pos + 1) (add_var id pos env)

(* Compute the closure environment *)

let rec add_positions entries pos_to_entry ~pos ~delta = function
  | [] -> entries, pos
  | id :: rem ->
    let entries =
      Ident.add id (pos_to_entry pos) entries
    in
    add_positions entries pos_to_entry ~pos:(pos + delta) ~delta rem

type function_definition =
  | Single_non_recursive
  | Multiple_recursive of Ident.t list

let closure_entries fun_defs fvs =
  let funct_entries, pos_end_functs =
    match fun_defs with
    | Single_non_recursive ->
      (* No need to store the function in the environment, but we still need to
         reserve a slot in the closure block *)
      Ident.empty, 3
    | Multiple_recursive functs ->
      add_positions Ident.empty (fun pos -> Function pos) ~pos:0 ~delta:3 functs
  in
  (* Note: [pos_end_functs] is the position where we would store the next
     function if there was one, and points after an eventual infix tag.
     Since that was the last function, we don't need the last infix tag
     and start storing free variables at [pos_end_functs - 1]. *)
  let all_entries, _end_pos =
    add_positions funct_entries (fun pos -> Free_variable pos)
      ~pos:(pos_end_functs - 1) ~delta:1 fvs
  in
  all_entries

(**** Examination of the continuation ****)

(* Return a label to the beginning of the given continuation.
   If the sequence starts with a branch, use the target of that branch
   as the label, thus avoiding a jump to a jump. *)

let label_code = function
    Kbranch lbl :: _ as cont -> (lbl, cont)
  | Klabel lbl :: _ as cont -> (lbl, cont)
  | cont -> let lbl = new_label() in (lbl, Klabel lbl :: cont)

(* Return a branch to the continuation. That is, an instruction that,
   when executed, branches to the continuation or performs what the
   continuation performs. We avoid generating branches to branches and
   branches to returns. *)

let rec make_branch_2 lbl n cont =
  function
    Kreturn m :: _ -> (Kreturn (n + m), cont)
  | Klabel _ :: c  -> make_branch_2 lbl n cont c
  | Kpop m :: c    -> make_branch_2 lbl (n + m) cont c
  | _              ->
      match lbl with
        Some lbl -> (Kbranch lbl, cont)
      | None     -> let lbl = new_label() in (Kbranch lbl, Klabel lbl :: cont)

let make_branch cont =
  match cont with
    (Kbranch _ as branch) :: _ -> (branch, cont)
  | (Kreturn _ as return) :: _ -> (return, cont)
  | Kraise k :: _ -> (Kraise k, cont)
  | Klabel lbl :: _ -> make_branch_2 (Some lbl) 0 cont cont
  | _ ->  make_branch_2 (None) 0 cont cont

(* Avoid a branch to a label that follows immediately *)

let branch_to label cont = match cont with
| Klabel label0::_ when label = label0 -> cont
| _ -> Kbranch label::cont

(* Discard all instructions up to the next label.
   This function is to be applied to the continuation before adding a
   non-terminating instruction (branch, raise, return) in front of it. *)

let rec discard_dead_code = function
    [] -> []
  | (Klabel _ | Krestart | Ksetglobal _) :: _ as cont -> cont
  | _ :: cont -> discard_dead_code cont

(* Check if we're in tailcall position *)

let rec is_tailcall = function
    Kreturn _ :: _ -> true
  | Klabel _ :: c -> is_tailcall c
  | Kpop _ :: c -> is_tailcall c
  | _ -> false

(* Will this primitive result in an OCaml call which would benefit
   from the tail call optimization? *)

let preserve_tailcall_for_prim = function
  | Popaque | Psequor | Psequand
  | Prunstack | Pperform | Presume | Preperform | Ppoll ->
      true
  | Pbytes_to_string | Pbytes_of_string | Pignore | Pgetglobal _ | Psetglobal _
  | Pmakeblock _ | Pfield _ | Pfield_computed | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Pduprecord _
  | Pccall _ | Praise _ | Pnot | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _ | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Poffsetint _ | Poffsetref _ | Pintoffloat
  | Pfloatofint | Pnegfloat | Pabsfloat | Paddfloat | Psubfloat | Pmulfloat
  | Pdivfloat | Pfloatcomp _ | Pstringlength | Pstringrefu  | Pstringrefs
  | Pcompare_ints | Pcompare_floats | Pcompare_bints _
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  | Pmakearray _ | Pduparray _ | Parraylength _ | Parrayrefu _ | Parraysetu _
  | Parrayrefs _ | Parraysets _ | Pisint | Pisout | Pbintofint _ | Pintofbint _
  | Pcvtbint _ | Pnegbint _ | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _
  | Pmodbint _ | Pandbint _ | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _
  | Pasrbint _ | Pbintcomp _ | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _
  | Pstring_load_16 _ | Pstring_load_32 _ | Pstring_load_64 _ | Pbytes_load_16 _
  | Pbytes_load_32 _ | Pbytes_load_64 _ | Pbytes_set_16 _ | Pbytes_set_32 _
  | Pbytes_set_64 _ | Pbigstring_load_16 _ | Pbigstring_load_32 _
  | Pbigstring_load_64 _ | Pbigstring_set_16 _ | Pbigstring_set_32 _
  | Pbigstring_set_64 _ | Pctconst _ | Pbswap16 | Pbbswap _ | Pint_as_pointer
  | Patomic_load
  | Pdls_get ->
      false

(* Add a Kpop N instruction in front of a continuation *)

let rec add_pop n cont =
  if n = 0 then cont else
    match cont with
      Kpop m :: cont -> add_pop (n + m) cont
    | Kreturn m :: cont -> Kreturn(n + m) :: cont
    | Kraise _ :: _ -> cont
    | _ -> Kpop n :: cont

(* Add the constant "unit" in front of a continuation *)

let add_const_unit = function
    (Kacc _ | Kconst _ | Kgetglobal _ | Kpush_retaddr _) :: _ as cont -> cont
  | cont -> Kconst const_unit :: cont

let rec push_dummies n k = match n with
| 0 -> k
| _ -> Kconst const_unit::Kpush::push_dummies (n-1) k


(**** Merging consecutive events ****)

let copy_event ev kind info repr =
  { ev with
    ev_pos = 0;                   (* patched in emitcode *)
    ev_kind = kind;
    ev_info = info;
    ev_repr = repr }

let merge_infos ev ev' =
  match ev.ev_info, ev'.ev_info with
    Event_other, info -> info
  | info, Event_other -> info
  | _                 -> fatal_error "Bytegen.merge_infos"

let merge_repr ev ev' =
  match ev.ev_repr, ev'.ev_repr with
    Event_none, x -> x
  | x, Event_none -> x
  | Event_parent r, Event_child r' when r == r' && !r = 1 -> Event_none
  | Event_child r, Event_parent r' when r == r' -> Event_parent r
  | _, _          -> fatal_error "Bytegen.merge_repr"

let merge_events ev ev' =
  let (maj, min) =
    match ev.ev_kind, ev'.ev_kind with
    (* Discard pseudo-events *)
      Event_pseudo,  _                              -> ev', ev
    | _,             Event_pseudo                   -> ev,  ev'
    (* Keep following event, supposedly more informative *)
    | Event_before,  (Event_after _ | Event_before) -> ev',  ev
    (* Discard following events, supposedly less informative *)
    | Event_after _, (Event_after _ | Event_before) -> ev, ev'
  in
  copy_event maj maj.ev_kind (merge_infos maj min) (merge_repr maj min)

let weaken_event ev cont =
  match ev.ev_kind with
    Event_after _ ->
      begin match cont with
        Kpush :: Kevent ({ev_repr = Event_none} as ev') :: c ->
          begin match ev.ev_info with
            Event_return _ ->
              (* Weaken event *)
              let repr = ref 1 in
              let ev =
                copy_event ev Event_pseudo ev.ev_info (Event_parent repr)
              and ev' =
                copy_event ev' ev'.ev_kind ev'.ev_info (Event_child repr)
              in
              Kevent ev :: Kpush :: Kevent ev' :: c
          | _ ->
              (* Only keep following event, equivalent *)
              cont
          end
      | _ ->
          Kevent ev :: cont
      end
  | _ ->
      Kevent ev :: cont

let add_event ev =
  function
    Kevent ev' :: cont -> weaken_event (merge_events ev ev') cont
  | cont               -> weaken_event ev cont

(* Pseudo events are ignored by the debugger. They are only used for
   generating backtraces.

   We prefer adding this event here rather than in lambda generation
   1) there are many different situations where a Pmakeblock can
      be generated
   2) we prefer inserting a pseudo event rather than an event after
      to prevent the debugger to stop at every single allocation. *)
let add_pseudo_event loc modname c =
  if !Clflags.debug then
    let ev_defname = string_of_scoped_location loc in
    let ev =
      { ev_pos = 0;                   (* patched in emitcode *)
        ev_module = modname;
        ev_loc = to_location loc;
        ev_defname;
        ev_kind = Event_pseudo;
        ev_info = Event_other;        (* Dummy *)
        ev_typenv = Env.Env_empty;    (* Dummy *)
        ev_typsubst = Subst.identity; (* Dummy *)
        ev_compenv = empty_env;       (* Dummy *)
        ev_stacksize = 0;             (* Dummy *)
        ev_repr = Event_none }        (* Dummy *)
    in
    add_event ev c
  else c

(**** Compilation of a lambda expression ****)

type stack_info = {
  try_blocks : int list;
  (* list of stack size for each nested try block *)
  sz_static_raises : (int * (int * int * int list)) list;
  (* association staticraise numbers -> (lbl,size of stack, try_blocks *)
  max_stack_used : int ref;
  (* Maximal stack size reached during the current function body *)
}

let create_stack_info () = {
  try_blocks = [];
  sz_static_raises = [];
  max_stack_used = ref 0
}

(* association staticraise numbers -> (lbl,size of stack, try_blocks *)

let push_static_raise stack_info i lbl_handler sz =
  { stack_info
    with
      sz_static_raises = (i, (lbl_handler, sz, stack_info.try_blocks))
                         :: stack_info.sz_static_raises
  }

let find_raise_label stack_info i =
  try
    List.assoc i stack_info.sz_static_raises
  with
  | Not_found ->
      Misc.fatal_error
        ("exit("^Int.to_string i^") outside appropriated catch")

(* Will the translation of l lead to a jump to label ? *)
let code_as_jump stack_info l sz = match l with
| Lstaticraise (i,[]) ->
    let label,size,tb = find_raise_label stack_info i in
    if sz = size && tb == stack_info.try_blocks then
      Some label
    else
      None
| _ -> None

(* Function bodies that remain to be compiled *)

type function_to_compile =
  { params: Ident.t list;               (* function parameters *)
    body: lambda;                       (* the function body *)
    label: label;                       (* the label of the function entry *)
    entries: closure_entry Ident.tbl;   (* the offsets for the free variables
                                           and mutually recursive functions *)
    rec_pos: int }                      (* rank in recursive definition *)

let functions_to_compile  = (Stack.create () : function_to_compile Stack.t)

(* Name of current compilation unit (for debugging events) *)

let compunit_name = ref ""

let check_stack stack_info sz =
  let curr = stack_info.max_stack_used in
  if sz > !curr then curr := sz

(* Sequence of string tests *)


(* Translate a primitive to a bytecode instruction (possibly a call to a C
   function) *)

let comp_bint_primitive bi suff args =
  let pref =
    match bi with Pnativeint -> "caml_nativeint_"
                | Pint32 -> "caml_int32_"
                | Pint64 -> "caml_int64_" in
  Kccall(pref ^ suff, List.length args)

let comp_primitive stack_info p sz args =
  check_stack stack_info sz;
  match p with
    Pgetglobal id -> Kgetglobal id
  | Psetglobal id -> Ksetglobal id
  | Pintcomp cmp -> Kintcomp cmp
  | Pcompare_ints -> Kccall("caml_int_compare", 2)
  | Pcompare_floats -> Kccall("caml_float_compare", 2)
  | Pcompare_bints bi -> comp_bint_primitive bi "compare" args
  | Pfield(n, _ptr, _mut) -> Kgetfield n
  | Pfield_computed -> Kgetvectitem
  | Psetfield(n, _ptr, _init) -> Ksetfield n
  | Psetfield_computed(_ptr, _init) -> Ksetvectitem
  | Psetfloatfield (n, _init) -> Ksetfloatfield n
  | Pduprecord _ -> Kccall("caml_obj_dup", 1)
  | Pccall p -> Kccall(p.prim_name, p.prim_arity)
  | Pperform ->
      check_stack stack_info (sz + 4);
      Kperform
  | Pnegint -> Knegint
  | Paddint -> Kaddint
  | Psubint -> Ksubint
  | Pmulint -> Kmulint
  | Pdivint _ -> Kdivint
  | Pmodint _ -> Kmodint
  | Pandint -> Kandint
  | Porint -> Korint
  | Pxorint -> Kxorint
  | Plslint -> Klslint
  | Plsrint -> Klsrint
  | Pasrint -> Kasrint
  | Poffsetint n -> Koffsetint n
  | Poffsetref n -> Koffsetref n
  | Pintoffloat -> Kccall("caml_int_of_float", 1)
  | Pfloatofint -> Kccall("caml_float_of_int", 1)
  | Pnegfloat -> Kccall("caml_neg_float", 1)
  | Pabsfloat -> Kccall("caml_abs_float", 1)
  | Paddfloat -> Kccall("caml_add_float", 2)
  | Psubfloat -> Kccall("caml_sub_float", 2)
  | Pmulfloat -> Kccall("caml_mul_float", 2)
  | Pdivfloat -> Kccall("caml_div_float", 2)
  | Pstringlength -> Kccall("caml_ml_string_length", 1)
  | Pbyteslength -> Kccall("caml_ml_bytes_length", 1)
  | Pstringrefs -> Kccall("caml_string_get", 2)
  | Pbytesrefs -> Kccall("caml_bytes_get", 2)
  | Pbytessets -> Kccall("caml_bytes_set", 3)
  | Pstringrefu -> Kgetstringchar
  | Pbytesrefu -> Kgetbyteschar
  | Pbytessetu -> Ksetbyteschar
  | Pstring_load_16(_) -> Kccall("caml_string_get16", 2)
  | Pstring_load_32(_) -> Kccall("caml_string_get32", 2)
  | Pstring_load_64(_) -> Kccall("caml_string_get64", 2)
  | Pbytes_set_16(_) -> Kccall("caml_bytes_set16", 3)
  | Pbytes_set_32(_) -> Kccall("caml_bytes_set32", 3)
  | Pbytes_set_64(_) -> Kccall("caml_bytes_set64", 3)
  | Pbytes_load_16(_) -> Kccall("caml_bytes_get16", 2)
  | Pbytes_load_32(_) -> Kccall("caml_bytes_get32", 2)
  | Pbytes_load_64(_) -> Kccall("caml_bytes_get64", 2)
  | Parraylength _ -> Kvectlength
  | Parrayrefs Pgenarray -> Kccall("caml_array_get", 2)
  | Parrayrefs Pfloatarray -> Kccall("caml_floatarray_get", 2)
  | Parrayrefs _ -> Kccall("caml_array_get_addr", 2)
  | Parraysets Pgenarray -> Kccall("caml_array_set", 3)
  | Parraysets Pfloatarray -> Kccall("caml_floatarray_set", 3)
  | Parraysets _ -> Kccall("caml_array_set_addr", 3)
  | Parrayrefu Pgenarray -> Kccall("caml_array_unsafe_get", 2)
  | Parrayrefu Pfloatarray -> Kccall("caml_floatarray_unsafe_get", 2)
  | Parrayrefu _ -> Kgetvectitem
  | Parraysetu Pgenarray -> Kccall("caml_array_unsafe_set", 3)
  | Parraysetu Pfloatarray -> Kccall("caml_floatarray_unsafe_set", 3)
  | Parraysetu _ -> Ksetvectitem
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Int_size -> "int_size"
       | Max_wosize -> "max_wosize"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin"
       | Backend_type -> "backend_type" in
     Kccall(Printf.sprintf "caml_sys_const_%s" const_name, 1)
  | Pisint -> Kisint
  | Pisout -> Kisout
  | Pbintofint bi -> comp_bint_primitive bi "of_int" args
  | Pintofbint bi -> comp_bint_primitive bi "to_int" args
  | Pcvtbint(src, dst) ->
      begin match (src, dst) with
      | (Pint32, Pnativeint) -> Kccall("caml_nativeint_of_int32", 1)
      | (Pnativeint, Pint32) -> Kccall("caml_nativeint_to_int32", 1)
      | (Pint32, Pint64) -> Kccall("caml_int64_of_int32", 1)
      | (Pint64, Pint32) -> Kccall("caml_int64_to_int32", 1)
      | (Pnativeint, Pint64) -> Kccall("caml_int64_of_nativeint", 1)
      | (Pint64, Pnativeint) -> Kccall("caml_int64_to_nativeint", 1)
      | ((Pint32 | Pint64 | Pnativeint), _) ->
          fatal_error "Bytegen.comp_primitive: invalid Pcvtbint cast"
      end
  | Pnegbint bi -> comp_bint_primitive bi "neg" args
  | Paddbint bi -> comp_bint_primitive bi "add" args
  | Psubbint bi -> comp_bint_primitive bi "sub" args
  | Pmulbint bi -> comp_bint_primitive bi "mul" args
  | Pdivbint { size = bi } -> comp_bint_primitive bi "div" args
  | Pmodbint { size = bi } -> comp_bint_primitive bi "mod" args
  | Pandbint bi -> comp_bint_primitive bi "and" args
  | Porbint bi -> comp_bint_primitive bi "or" args
  | Pxorbint bi -> comp_bint_primitive bi "xor" args
  | Plslbint bi -> comp_bint_primitive bi "shift_left" args
  | Plsrbint bi -> comp_bint_primitive bi "shift_right_unsigned" args
  | Pasrbint bi -> comp_bint_primitive bi "shift_right" args
  | Pbintcomp(_, Ceq) -> Kccall("caml_equal", 2)
  | Pbintcomp(_, Cne) -> Kccall("caml_notequal", 2)
  | Pbintcomp(_, Clt) -> Kccall("caml_lessthan", 2)
  | Pbintcomp(_, Cgt) -> Kccall("caml_greaterthan", 2)
  | Pbintcomp(_, Cle) -> Kccall("caml_lessequal", 2)
  | Pbintcomp(_, Cge) -> Kccall("caml_greaterequal", 2)
  | Pbigarrayref(_, n, _, _) -> Kccall("caml_ba_get_" ^ Int.to_string n, n + 1)
  | Pbigarrayset(_, n, _, _) -> Kccall("caml_ba_set_" ^ Int.to_string n, n + 2)
  | Pbigarraydim(n) -> Kccall("caml_ba_dim_" ^ Int.to_string n, 1)
  | Pbigstring_load_16(_) -> Kccall("caml_ba_uint8_get16", 2)
  | Pbigstring_load_32(_) -> Kccall("caml_ba_uint8_get32", 2)
  | Pbigstring_load_64(_) -> Kccall("caml_ba_uint8_get64", 2)
  | Pbigstring_set_16(_) -> Kccall("caml_ba_uint8_set16", 3)
  | Pbigstring_set_32(_) -> Kccall("caml_ba_uint8_set32", 3)
  | Pbigstring_set_64(_) -> Kccall("caml_ba_uint8_set64", 3)
  | Pbswap16 -> Kccall("caml_bswap16", 1)
  | Pbbswap(bi) -> comp_bint_primitive bi "bswap" args
  | Pint_as_pointer -> Kccall("caml_int_as_pointer", 1)
  | Pbytes_to_string -> Kccall("caml_string_of_bytes", 1)
  | Pbytes_of_string -> Kccall("caml_bytes_of_string", 1)
  | Patomic_load -> Kccall("caml_atomic_load", 1)
  | Pdls_get -> Kccall("caml_domain_dls_get", 1)
  | Ppoll -> Kccall("caml_process_pending_actions_with_root", 1)
  (* The cases below are handled in [comp_expr] before the [comp_primitive] call
     (in the order in which they appear below),
     so they should never be reached in this function. *)
  | Prunstack | Presume | Preperform
  | Pignore | Popaque
  | Pnot | Psequand | Psequor
  | Praise _
  | Pmakearray _ | Pduparray _
  | Pfloatcomp _
  | Pmakeblock _
  | Pfloatfield _
    ->
      fatal_error "Bytegen.comp_primitive"

let is_immed n = immed_min <= n && n <= immed_max

module Storer =
  Switch.Store
    (struct type t = lambda type key = lambda
      let compare_key = Stdlib.compare
      let make_key = Lambda.make_key end)

(* Compile an expression.
   The value of the expression is left in the accumulator.
   env = compilation environment
   exp = the lambda expression to compile
   sz = current size of the stack frame
   cont = list of instructions to execute afterwards
   Result = list of instructions that evaluate exp, then perform cont. *)

let rec comp_expr stack_info env exp sz cont =
  check_stack stack_info sz;
  match exp with
    Lvar id | Lmutvar id ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        Kacc(sz - pos) :: cont
      with Not_found ->
      let not_found () =
        fatal_error ("Bytegen.comp_expr: var " ^ Ident.unique_name id)
      in
      match env.ce_closure with
      | Not_in_closure -> not_found ()
      | In_closure { entries; env_pos } ->
        match Ident.find_same id entries with
        | Free_variable pos ->
          Kenvacc(pos - env_pos) :: cont
        | Function pos ->
          Koffsetclosure(pos - env_pos) :: cont
        | exception Not_found -> not_found ()
      end
  | Lconst cst ->
      Kconst cst :: cont
  | Lapply{ap_func = func; ap_args = args} ->
      let nargs = List.length args in
      if is_tailcall cont then begin
        comp_args stack_info env args sz
          (Kpush :: comp_expr stack_info env func (sz + nargs)
            (Kappterm(nargs, sz + nargs) :: discard_dead_code cont))
      end else begin
        if nargs < 4 then
          comp_args stack_info env args sz
            (Kpush ::
             comp_expr stack_info env func (sz + nargs) (Kapply nargs :: cont))
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args stack_info env args (sz + 3)
            (Kpush :: comp_expr stack_info env func (sz + 3 + nargs)
                      (Kapply nargs :: cont1))
        end
      end
  | Lsend(kind, met, obj, args, _) ->
      assert (kind <> Cached);
      let nargs = List.length args + 1 in
      let getmethod, args' =
        if kind = Self then (Kgetmethod, met::obj::args) else
        match met with
          Lconst(Const_base(Const_int n)) -> (Kgetpubmet n, obj::args)
        | _ -> (Kgetdynmet, met::obj::args)
      in
      if is_tailcall cont then
        comp_args stack_info env args' sz
          (getmethod :: Kappterm(nargs, sz + nargs) :: discard_dead_code cont)
      else
        if nargs < 4 then
          comp_args stack_info env args' sz
            (getmethod :: Kapply nargs :: cont)
        else begin
          let (lbl, cont1) = label_code cont in
          Kpush_retaddr lbl ::
          comp_args stack_info env args' (sz + 3)
            (getmethod :: Kapply nargs :: cont1)
        end
  | Lfunction{params; body; loc} -> (* assume kind = Curried *)
      let cont = add_pseudo_event loc !compunit_name cont in
      let lbl = new_label() in
      let fv = Ident.Set.elements(free_variables exp) in
      let entries = closure_entries Single_non_recursive fv in
      let to_compile =
        { params = List.map fst params; body = body; label = lbl;
          entries = entries; rec_pos = 0 } in
      Stack.push to_compile functions_to_compile;
      comp_args stack_info env (List.map (fun n -> Lvar n) fv) sz
        (Kclosure(lbl, List.length fv) :: cont)
  | Llet(_, _k, id, arg, body)
  | Lmutlet(_k, id, arg, body) ->
      comp_expr stack_info env arg sz
        (Kpush :: comp_expr stack_info (add_var id (sz+1) env) body (sz+1)
          (add_pop 1 cont))
  | Lletrec(decl, body) ->
      let ndecl = List.length decl in
      let fv =
        Ident.Set.elements (free_variables (Lletrec(decl, lambda_unit))) in
      let rec_idents = List.map (fun { id } -> id) decl in
      let entries =
        closure_entries (Multiple_recursive rec_idents) fv
      in
      let rec comp_fun pos = function
          [] -> []
        | { def = {params; body} } :: rem ->
            let lbl = new_label() in
            let to_compile =
              { params = List.map fst params; body = body; label = lbl;
                entries = entries; rec_pos = pos} in
            Stack.push to_compile functions_to_compile;
            lbl :: comp_fun (pos + 1) rem
      in
      let lbls = comp_fun 0 decl in
      comp_args stack_info env (List.map (fun n -> Lvar n) fv) sz
        (Kclosurerec(lbls, List.length fv) ::
         (comp_expr stack_info
            (add_vars rec_idents (sz+1) env) body (sz + ndecl)
            (add_pop ndecl cont)))
  | Lprim(Popaque, [arg], _) ->
      comp_expr stack_info env arg sz cont
  | Lprim(Pignore, [arg], _) ->
      comp_expr stack_info env arg sz (add_const_unit cont)
  | Lprim(Pnot, [arg], _) ->
      let newcont =
        match cont with
          Kbranchif lbl :: cont1 -> Kbranchifnot lbl :: cont1
        | Kbranchifnot lbl :: cont1 -> Kbranchif lbl :: cont1
        | _ -> Kboolnot :: cont in
      comp_expr stack_info env arg sz newcont
  | Lprim(Psequand, [exp1; exp2], _) ->
      begin match cont with
        Kbranchifnot lbl :: _ ->
          comp_expr stack_info env exp1 sz (Kbranchifnot lbl ::
            comp_expr stack_info env exp2 sz cont)
      | Kbranchif lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr stack_info env exp1 sz (Kbranchifnot lbl2 ::
            comp_expr stack_info env exp2 sz (Kbranchif lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr stack_info env exp1 sz (Kstrictbranchifnot lbl ::
            comp_expr stack_info env exp2 sz cont1)
      end
  | Lprim(Psequor, [exp1; exp2], _) ->
      begin match cont with
        Kbranchif lbl :: _ ->
          comp_expr stack_info env exp1 sz (Kbranchif lbl ::
            comp_expr stack_info env exp2 sz cont)
      | Kbranchifnot lbl :: cont1 ->
          let (lbl2, cont2) = label_code cont1 in
          comp_expr stack_info env exp1 sz (Kbranchif lbl2 ::
            comp_expr stack_info env exp2 sz (Kbranchifnot lbl :: cont2))
      | _ ->
          let (lbl, cont1) = label_code cont in
          comp_expr stack_info env exp1 sz (Kstrictbranchif lbl ::
            comp_expr stack_info env exp2 sz cont1)
      end
  | Lprim(Praise k, [arg], _) ->
      comp_expr stack_info env arg sz (Kraise k :: discard_dead_code cont)
  | Lprim(Paddint, [arg; Lconst(Const_base(Const_int n))], _)
    when is_immed n ->
      comp_expr stack_info env arg sz (Koffsetint n :: cont)
  | Lprim(Psubint, [arg; Lconst(Const_base(Const_int n))], _)
    when is_immed (-n) ->
      comp_expr stack_info env arg sz (Koffsetint (-n) :: cont)
  | Lprim (Poffsetint n, [arg], _)
    when not (is_immed n) ->
      comp_expr stack_info env arg sz
        (Kpush::
         Kconst (Const_base (Const_int n))::
         Kaddint::cont)
  | Lprim(Pmakearray (kind, _), args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      begin match kind with
        Pintarray | Paddrarray ->
          comp_args stack_info env args sz
            (Kmakeblock(List.length args, 0) :: cont)
      | Pfloatarray ->
          comp_args stack_info env args sz
            (Kmakefloatblock(List.length args) :: cont)
      | Pgenarray ->
          if args = []
          then Kmakeblock(0, 0) :: cont
          else comp_args stack_info env args sz
                 (Kmakeblock(List.length args, 0) ::
                  Kccall("caml_array_of_uniform_array", 1) :: cont)
      end
  | Lprim(Presume, args, _) ->
      let nargs = List.length args - 1 in
      assert (nargs = 3);
      if is_tailcall cont then begin
        (* Resumeterm itself only pushes 2 words, but perform adds another *)
        check_stack stack_info 3;
        comp_args stack_info env args sz
          (Kresumeterm(sz + nargs) :: discard_dead_code cont)
      end else begin
        (* Resume itself only pushes 2 words, but perform adds another *)
        check_stack stack_info (sz + nargs + 3);
        comp_args stack_info env args sz (Kresume :: cont)
      end
  | Lprim(Prunstack, args, _) ->
      let nargs = List.length args in
      assert (nargs = 3);
      if is_tailcall cont then begin
        (* Resumeterm itself only pushes 2 words, but perform adds another *)
        check_stack stack_info 3;
        Kconst const_unit :: Kpush ::
          comp_args stack_info env args (sz + 1)
          (Kresumeterm(sz + nargs) :: discard_dead_code cont)
      end else begin
        (* Resume itself only pushes 2 words, but perform adds another *)
        check_stack stack_info (sz + nargs + 3);
        Kconst const_unit :: Kpush ::
          comp_args stack_info env args (sz + 1) (Kresume :: cont)
      end
  | Lprim(Preperform, args, _) ->
      let nargs = List.length args - 1 in
      assert (nargs = 2);
      check_stack stack_info (sz + 3);
      if is_tailcall cont then
        comp_args stack_info env args sz
          (Kreperformterm(sz + nargs) :: discard_dead_code cont)
      else
        fatal_error "Reperform used in non-tail position"
  | Lprim (Pduparray (kind, mutability),
           [Lprim (Pmakearray (kind',_),args,_)], loc) ->
      assert (kind = kind');
      comp_expr stack_info env
        (Lprim (Pmakearray (kind, mutability), args, loc)) sz cont
  | Lprim (Pduparray _, [arg], loc) ->
      let prim_obj_dup =
        Primitive.simple ~name:"caml_obj_dup" ~arity:1 ~alloc:true
      in
      comp_expr stack_info env (Lprim (Pccall prim_obj_dup, [arg], loc)) sz cont
  | Lprim (Pduparray _, _, _) ->
      Misc.fatal_error "Bytegen.comp_expr: Pduparray takes exactly one arg"
(* Integer first for enabling further optimization (cf. emitcode.ml)  *)
  | Lprim (Pintcomp c, [arg ; (Lconst _ as k)], _) ->
      let p = Pintcomp (swap_integer_comparison c)
      and args = [k ; arg] in
      let nargs = List.length args - 1 in
      comp_args stack_info env args sz
        (comp_primitive stack_info p (sz + nargs - 1) args :: cont)
  | Lprim (Pfloatcomp cmp, args, _) ->
      let cont =
        match cmp with
        | CFeq -> Kccall("caml_eq_float", 2) :: cont
        | CFneq -> Kccall("caml_neq_float", 2) :: cont
        | CFlt -> Kccall("caml_lt_float", 2) :: cont
        | CFnlt -> Kccall("caml_lt_float", 2) :: Kboolnot :: cont
        | CFgt -> Kccall("caml_gt_float", 2) :: cont
        | CFngt -> Kccall("caml_gt_float", 2) :: Kboolnot :: cont
        | CFle -> Kccall("caml_le_float", 2) :: cont
        | CFnle -> Kccall("caml_le_float", 2) :: Kboolnot :: cont
        | CFge -> Kccall("caml_ge_float", 2) :: cont
        | CFnge -> Kccall("caml_ge_float", 2) :: Kboolnot :: cont
      in
      comp_args stack_info env args sz cont
  | Lprim(Pmakeblock(tag, _mut, _), args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args stack_info env args sz
        (Kmakeblock(List.length args, tag) :: cont)
  | Lprim(Pfloatfield n, args, loc) ->
      let cont = add_pseudo_event loc !compunit_name cont in
      comp_args stack_info env args sz (Kgetfloatfield n :: cont)
  | Lprim(p, args, _) ->
      let nargs = List.length args - 1 in
      comp_args stack_info env args sz
        (comp_primitive stack_info p (sz + nargs - 1) args :: cont)
  | Lstaticcatch (body, (i, vars) , handler) ->
      let vars = List.map fst vars in
      let nvars = List.length vars in
      let branch1, cont1 = make_branch cont in
      let r =
        if nvars <> 1 then begin (* general case *)
          let lbl_handler, cont2 =
            label_code
              (comp_expr
                stack_info
                (add_vars vars (sz+1) env)
                handler (sz+nvars) (add_pop nvars cont1)) in
          let stack_info =
            push_static_raise stack_info i lbl_handler (sz+nvars) in
          push_dummies nvars
            (comp_expr stack_info env body (sz+nvars)
            (add_pop nvars (branch1 :: cont2)))
        end else begin (* small optimization for nvars = 1 *)
          let var = match vars with [var] -> var | _ -> assert false in
          let lbl_handler, cont2 =
            label_code
              (Kpush::comp_expr stack_info
                (add_var var (sz+1) env)
                handler (sz+1) (add_pop 1 cont1)) in
          let stack_info =
            push_static_raise stack_info i lbl_handler sz in
          comp_expr stack_info env body sz (branch1 :: cont2)
        end in
      r
  | Lstaticraise (i, args) ->
      let cont = discard_dead_code cont in
      let label,size,tb = find_raise_label stack_info i in
      let cont = branch_to label cont in
      let rec loop sz tbb =
        if tb == tbb then add_pop (sz-size) cont
        else match tbb with
        | [] -> assert false
        | try_sz :: tbb -> add_pop (sz-try_sz-4) (Kpoptrap :: loop try_sz tbb)
      in
      let cont = loop sz stack_info.try_blocks in
      begin match args with
      | [arg] -> (* optim, argument passed in accumulator *)
          comp_expr stack_info env arg sz cont
      | _ -> comp_exit_args stack_info env args sz size cont
      end
  | Ltrywith(body, id, handler) ->
      let (branch1, cont1) = make_branch cont in
      let lbl_handler = new_label() in
      let body_cont =
        Kpoptrap :: branch1 ::
        Klabel lbl_handler :: Kpush ::
        comp_expr
          stack_info (add_var id (sz+1) env) handler (sz+1) (add_pop 1 cont1)
      in
      let stack_info =
        { stack_info with try_blocks = sz :: stack_info.try_blocks } in
      let l = comp_expr stack_info env body (sz+4) body_cont in
      Kpushtrap lbl_handler :: l
  | Lifthenelse(cond, ifso, ifnot) ->
      comp_binary_test stack_info env cond ifso ifnot sz cont
  | Lsequence(exp1, exp2) ->
      comp_expr stack_info env exp1 sz (comp_expr stack_info env exp2 sz cont)
  | Lwhile(cond, body) ->
      let lbl_loop = new_label() in
      let lbl_test = new_label() in
      Kbranch lbl_test :: Klabel lbl_loop :: Kcheck_signals ::
        comp_expr stack_info env body sz
          (Klabel lbl_test ::
           comp_expr stack_info env cond sz
             (Kbranchif lbl_loop :: add_const_unit cont))
  | Lfor(param, start, stop, dir, body) ->
      let lbl_loop = new_label() in
      let lbl_exit = new_label() in
      let offset = match dir with Upto -> 1 | Downto -> -1 in
      let comp = match dir with Upto -> Cgt | Downto -> Clt in
      comp_expr stack_info env start sz
        (Kpush :: comp_expr stack_info env stop (sz+1)
          (Kpush :: Kpush :: Kacc 2 :: Kintcomp comp :: Kbranchif lbl_exit ::
           Klabel lbl_loop :: Kcheck_signals ::
           comp_expr stack_info (add_var param (sz+1) env) body (sz+2)
             (Kacc 1 :: Kpush :: Koffsetint offset :: Kassign 2 ::
              Kacc 1 :: Kintcomp Cne :: Kbranchif lbl_loop ::
              Klabel lbl_exit :: add_const_unit (add_pop 2 cont))))
  | Lswitch(arg, sw, _loc) ->
      let (branch, cont1) = make_branch cont in
      let c = ref (discard_dead_code cont1) in

(* Build indirection vectors *)
      let store = Storer.mk_store () in
      let act_consts = Array.make sw.sw_numconsts 0
      and act_blocks = Array.make sw.sw_numblocks 0 in
      begin match sw.sw_failaction with (* default is index 0 *)
      | Some fail -> ignore (store.act_store () fail)
      | None      -> ()
      end ;
      List.iter
        (fun (n, act) -> act_consts.(n) <- store.act_store () act) sw.sw_consts;
      List.iter
        (fun (n, act) -> act_blocks.(n) <- store.act_store () act) sw.sw_blocks;
(* Compile and label actions *)
      let acts = store.act_get () in
(*
      let a = store.act_get_shared () in
      Array.iter
        (function
          | Switch.Shared (Lstaticraise _) -> ()
          | Switch.Shared act ->
              Printlambda.lambda Format.str_formatter act ;
              Printf.eprintf "SHARE BYTE:\n%s\n" (Format.flush_str_formatter ())
          | _ -> ())
        a ;
*)
      let lbls = Array.make (Array.length acts) 0 in
      for i = Array.length acts-1 downto 0 do
        let lbl,c1 =
          label_code (comp_expr stack_info env acts.(i) sz (branch :: !c)) in
        lbls.(i) <- lbl ;
        c := discard_dead_code c1
      done ;

(* Build label vectors *)
      let lbl_blocks = Array.make sw.sw_numblocks 0 in
      for i = sw.sw_numblocks - 1 downto 0 do
        lbl_blocks.(i) <- lbls.(act_blocks.(i))
      done;
      let lbl_consts = Array.make sw.sw_numconsts 0 in
      for i = sw.sw_numconsts - 1 downto 0 do
        lbl_consts.(i) <- lbls.(act_consts.(i))
      done;
      comp_expr stack_info env arg sz (Kswitch(lbl_consts, lbl_blocks) :: !c)
  | Lstringswitch (arg,sw,d,loc) ->
      comp_expr stack_info env
        (Matching.expand_stringswitch loc arg sw d) sz cont
  | Lassign(id, expr) ->
      begin try
        let pos = Ident.find_same id env.ce_stack in
        comp_expr stack_info env expr sz (Kassign(sz - pos) :: cont)
      with Not_found ->
        fatal_error "Bytegen.comp_expr: assign"
      end
  | Levent(lam, lev) ->
      let ev_defname = string_of_scoped_location lev.lev_loc in
      let event kind info =
        { ev_pos = 0;                   (* patched in emitcode *)
          ev_module = !compunit_name;
          ev_loc = to_location lev.lev_loc;
          ev_kind = kind;
          ev_defname;
          ev_info = info;
          ev_typenv = Env.summary lev.lev_env;
          ev_typsubst = Subst.identity;
          ev_compenv = env;
          ev_stacksize = sz;
          ev_repr =
            begin match lev.lev_repr with
              None ->
                Event_none
            | Some ({contents = 1} as repr) when lev.lev_kind = Lev_function ->
                Event_child repr
            | Some ({contents = 1} as repr) ->
                Event_parent repr
            | Some repr when lev.lev_kind = Lev_function ->
                Event_parent repr
            | Some repr ->
                Event_child repr
            end }
      in
      begin match lev.lev_kind with
        Lev_before ->
          let c = comp_expr stack_info env lam sz cont in
          let ev = event Event_before Event_other in
          add_event ev c
      | Lev_function ->
          let c = comp_expr stack_info env lam sz cont in
          let ev = event Event_pseudo Event_function in
          add_event ev c
      | Lev_pseudo ->
          let c = comp_expr stack_info env lam sz cont in
          let ev = event Event_pseudo Event_other in
          add_event ev c
      | Lev_after ty ->
          let preserve_tailcall =
            match lam with
            | Lprim(prim, _, _) -> preserve_tailcall_for_prim prim
            | _ -> true
          in
          if preserve_tailcall && is_tailcall cont then
            (* don't destroy tail call opt *)
            comp_expr stack_info env lam sz cont
          else begin
            let info =
              match lam with
                Lapply{ap_args = args}  -> Event_return (List.length args)
              | Lsend(_, _, _, args, _) -> Event_return (List.length args + 1)
              | Lprim(_,args,_)         -> Event_return (List.length args)
              | _                       -> Event_other
            in
            let ev = event (Event_after ty) info in
            let cont1 = add_event ev cont in
            comp_expr stack_info env lam sz cont1
          end
      end
  | Lifused (_, exp) ->
      comp_expr stack_info env exp sz cont

(* Compile a list of arguments [e1; ...; eN] to a primitive operation.
   The values of eN ... e2 are pushed on the stack, e2 at top of stack,
   then e3, then ... The value of e1 is left in the accumulator. *)

and comp_args stack_info env argl sz cont =
  comp_expr_list stack_info env (List.rev argl) sz cont

and comp_expr_list stack_info env exprl sz cont = match exprl with
    [] -> cont
  | [exp] -> comp_expr stack_info env exp sz cont
  | exp :: rem ->
      comp_expr stack_info env exp sz
        (Kpush :: comp_expr_list stack_info env rem (sz+1) cont)

and comp_exit_args stack_info env argl sz pos cont =
   comp_expr_list_assign stack_info env (List.rev argl) sz pos cont

and comp_expr_list_assign stack_info env exprl sz pos cont = match exprl with
  | [] -> cont
  | exp :: rem ->
      comp_expr stack_info env exp sz
        (Kassign (sz-pos)
         ::comp_expr_list_assign stack_info env rem sz (pos-1) cont)

(* Compile an if-then-else test. *)

and comp_binary_test stack_info env cond ifso ifnot sz cont =
  let cont_cond =
    if ifnot = Lconst const_unit then begin
      let (lbl_end, cont1) = label_code cont in
      Kstrictbranchifnot lbl_end :: comp_expr stack_info env ifso sz cont1
    end else
    match code_as_jump stack_info ifso sz with
    | Some label ->
      let cont = comp_expr stack_info env ifnot sz cont in
      Kbranchif label :: cont
    | None ->
        match code_as_jump stack_info ifnot sz with
        | Some label ->
            let cont = comp_expr stack_info env ifso sz cont in
            Kbranchifnot label :: cont
        | None ->
            let (branch_end, cont1) = make_branch cont in
            let (lbl_not, cont2) =
              label_code(comp_expr stack_info env ifnot sz cont1) in
            Kbranchifnot lbl_not ::
            comp_expr stack_info env ifso sz (branch_end :: cont2) in

  comp_expr stack_info env cond sz cont_cond

(**** Compilation of a code block (with tracking of stack usage) ****)

let comp_block env exp sz cont =
  let stack_info = create_stack_info () in
  let code = comp_expr stack_info env exp sz cont in
  let used_safe = !(stack_info.max_stack_used) + Config.stack_safety_margin in
  if used_safe > Config.stack_threshold then
    Kconst(Const_base(Const_int used_safe)) ::
    Kccall("caml_ensure_stack_capacity", 1) ::
    code
  else
    code

(**** Compilation of functions ****)

let comp_function tc cont =
  let arity = List.length tc.params in
  let ce_stack, _last_pos =
    add_positions Ident.empty Fun.id ~pos:arity ~delta:(-1) tc.params
  in
  let env =
    { ce_stack;
      ce_closure =
        In_closure { entries = tc.entries; env_pos = 3 * tc.rec_pos }
    }
  in
  let cont =
    comp_block env tc.body arity (Kreturn arity :: cont) in
  if arity > 1 then
    Krestart :: Klabel tc.label :: Kgrab(arity - 1) :: cont
  else
    Klabel tc.label :: cont

let comp_remainder cont =
  let c = ref cont in
  begin try
    while true do
      c := comp_function (Stack.pop functions_to_compile) !c
    done
  with Stack.Empty ->
    ()
  end;
  !c

(**** Compilation of a lambda phrase ****)

let reset () =
  label_counter := 0;
  compunit_name := "";
  Stack.clear functions_to_compile

let compile_gen ?modulename ~init_stack expr =
  reset ();
  begin match modulename with
  | Some name -> compunit_name := name
  | None -> ()
  end;
  Fun.protect ~finally:reset (fun () ->
  let init_code = comp_block empty_env expr init_stack [] in
  if Stack.length functions_to_compile > 0 then begin
    let lbl_init = new_label() in
    (Kbranch lbl_init :: comp_remainder (Klabel lbl_init :: init_code)),
    false
  end else
    init_code, true)

let compile_implementation modulename expr =
  fst (compile_gen ~modulename ~init_stack:0 expr)

let compile_phrase expr =
  compile_gen ~init_stack:1 expr
