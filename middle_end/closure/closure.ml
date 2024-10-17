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

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
open Primitive
open Lambda
open Switch
open Clambda
module P = Clambda_primitives

module Int = Numbers.Int
module Storer =
  Switch.Store
    (struct
      type t = lambda
      type key = lambda
      let make_key =  Lambda.make_key
      let compare_key = Stdlib.compare
    end)

module V = Backend_var
module VP = Backend_var.With_provenance

(* The current backend *)

let no_phantom_lets () =
  Misc.fatal_error "Closure does not support phantom let generation"

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

(* Auxiliary for accessing globals.  We change the name of the global
   to the name of the corresponding asm symbol.  This is done here
   and no longer in Cmmgen so that approximations stored in .cmx files
   contain the right names if the -for-pack option is active. *)

let getglobal dbg id =
  Uprim(P.Pread_symbol (Compilenv.symbol_for_global id), [], dbg)

(* Check if a variable occurs in a [clambda] term. *)

let occurs_var var u =
  let rec occurs = function
      Uvar v -> v = var
    | Uconst _ -> false
    | Udirect_apply(_lbl, args, _) -> List.exists occurs args
    | Ugeneric_apply(funct, args, _) -> occurs funct || List.exists occurs args
    | Uclosure(_fundecls, clos) -> List.exists occurs clos
    | Uoffset(u, _ofs) -> occurs u
    | Ulet(_str, _kind, _id, def, body) -> occurs def || occurs body
    | Uphantom_let _ -> no_phantom_lets ()
    | Uprim(_p, args, _) -> List.exists occurs args
    | Uswitch(arg, s, _dbg) ->
        occurs arg ||
        occurs_array s.us_actions_consts || occurs_array s.us_actions_blocks
    | Ustringswitch(arg,sw,d) ->
        occurs arg ||
        List.exists (fun (_,e) -> occurs e) sw ||
        (match d with None -> false | Some d -> occurs d)
    | Ustaticfail (_, args) -> List.exists occurs args
    | Ucatch(_, _, body, hdlr) -> occurs body || occurs hdlr
    | Utrywith(body, _exn, hdlr) -> occurs body || occurs hdlr
    | Uifthenelse(cond, ifso, ifnot) ->
        occurs cond || occurs ifso || occurs ifnot
    | Usequence(u1, u2) -> occurs u1 || occurs u2
    | Uwhile(cond, body) -> occurs cond || occurs body
    | Ufor(_id, lo, hi, _dir, body) -> occurs lo || occurs hi || occurs body
    | Uassign(id, u) -> id = var || occurs u
    | Usend(_, met, obj, args, _) ->
        occurs met || occurs obj || List.exists occurs args
    | Uunreachable -> false
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  let open Clambda_primitives in
  match prim with
  | Pread_symbol _ -> 1
  | Pmakeblock _ -> 5 + List.length args
  | Pfield _ -> 1
  | Psetfield(_f, isptr, init) ->
    begin match init with
    | Root_initialization -> 1  (* never causes a write barrier hit *)
    | Assignment | Heap_initialization ->
      match isptr with
      | Pointer -> 4
      | Immediate -> 1
    end
  | Pfloatfield _ -> 1
  | Psetfloatfield _ -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength -> 5
  | Pbyteslength -> 5
  | Pstringrefs  -> 6
  | Pbytesrefs | Pbytessets -> 6
  | Pmakearray _ -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbigarrayref(_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(_, ndims, _, _) -> 4 + ndims * 6
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of switch cost *)

let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Uvar _ -> ()
    | Uconst _ -> incr size
    | Udirect_apply(_, args, _) ->
        size := !size + 4; lambda_list_size args
    | Ugeneric_apply(fn, args, _) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | Uclosure _ ->
        raise Exit (* inlining would duplicate function definitions *)
    | Uoffset(lam, _ofs) ->
        incr size; lambda_size lam
    | Ulet(_str, _kind, _id, lam, body) ->
        lambda_size lam; lambda_size body
    | Uphantom_let _ -> no_phantom_lets ()
    | Uprim(prim, args, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Uswitch(lam, cases, _dbg) ->
        if Array.length cases.us_actions_consts > 1 then size := !size + 5 ;
        if Array.length cases.us_actions_blocks > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.us_actions_consts ;
        lambda_array_size cases.us_actions_blocks
    | Ustringswitch (lam,sw,d) ->
        lambda_size lam ;
       (* as ifthenelse *)
        List.iter
          (fun (_,lam) ->
            size := !size+2 ;
            lambda_size lam)
          sw ;
        Option.iter lambda_size d
    | Ustaticfail (_,args) -> lambda_list_size args
    | Ucatch(_, _, body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | Utrywith(body, _id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Uifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Usequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Uwhile(cond, body) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ufor(_id, low, high, _dir, body) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Uassign(_id, lam) ->
        incr size;  lambda_size lam
    | Usend(_, met, obj, args, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
    | Uunreachable -> ()
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false

let is_pure_prim p =
  let open Semantics_of_primitives in
  match Semantics_of_primitives.for_primitive p with
  | (No_effects | Only_generative_effects), _ -> true
  | Arbitrary_effects, _ -> false

(* Check if a clambda term is ``pure'',
   that is without side-effects *and* not containing function definitions
   (Pure terms may still read mutable state) *)

let rec is_pure = function
    Uvar _ -> true
  | Uconst _ -> true
  | Uprim(p, args, _) -> is_pure_prim p && List.for_all is_pure args
  | Uoffset(arg, _) -> is_pure arg
  | Ulet(Immutable, _, _var, def, body) ->
      is_pure def && is_pure body
  | _ -> false

(* Simplify primitive operations on known arguments *)

let make_const c = (Uconst c, Value_const c)
let make_const_ref c =
  make_const(Uconst_ref(Compilenv.new_structured_constant ~shared:true c,
    Some c))
let make_const_int n = make_const (Uconst_int n)
let make_const_bool b = make_const_int(if b then 1 else 0)

let make_integer_comparison cmp x y =
  let open Clambda_primitives in
  make_const_bool
    (match cmp with
       Ceq -> x = y
     | Cne -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)

let make_float_comparison cmp x y =
  make_const_bool
    (match cmp with
     | CFeq -> x = y
     | CFneq -> not (x = y)
     | CFlt -> x < y
     | CFnlt -> not (x < y)
     | CFgt -> x > y
     | CFngt -> not (x > y)
     | CFle -> x <= y
     | CFnle -> not (x <= y)
     | CFge -> x >= y
     | CFnge -> not (x >= y))

let make_const_float n = make_const_ref (Uconst_float n)
let make_const_natint n = make_const_ref (Uconst_nativeint n)
let make_const_int32 n = make_const_ref (Uconst_int32 n)
let make_const_int64 n = make_const_ref (Uconst_int64 n)

(* The [fpc] parameter is true if constant propagation of
   floating-point computations is allowed *)

let simplif_arith_prim_pure ~backend fpc p (args, approxs) dbg =
  let module B = (val backend : Backend_intf.S) in
  let open Clambda_primitives in
  let default = (Uprim(p, args, dbg), Value_unknown) in
  match approxs with
  (* int (or enumerated type) *)
  | [ Value_const(Uconst_int n1) ] ->
      begin match p with
      | Pnot -> make_const_bool (n1 = 0)
      | Pnegint -> make_const_int (- n1)
      | Poffsetint n -> make_const_int (n + n1)
      | Pfloatofint when fpc -> make_const_float (float_of_int n1)
      | Pbintofint Pnativeint -> make_const_natint (Nativeint.of_int n1)
      | Pbintofint Pint32 -> make_const_int32 (Int32.of_int n1)
      | Pbintofint Pint64 -> make_const_int64 (Int64.of_int n1)
      | Pbswap16 -> make_const_int (((n1 land 0xff) lsl 8)
                                    lor ((n1 land 0xff00) lsr 8))
      | _ -> default
      end
  (* int (or enumerated type), int (or enumerated type) *)
  | [ Value_const(Uconst_int n1);
      Value_const(Uconst_int n2) ] ->
      begin match p with
      | Psequand -> make_const_bool (n1 <> 0 && n2 <> 0)
      | Psequor -> make_const_bool (n1 <> 0 || n2 <> 0)
      | Paddint -> make_const_int (n1 + n2)
      | Psubint -> make_const_int (n1 - n2)
      | Pmulint -> make_const_int (n1 * n2)
      | Pdivint _ when n2 <> 0 -> make_const_int (n1 / n2)
      | Pmodint _ when n2 <> 0 -> make_const_int (n1 mod n2)
      | Pandint -> make_const_int (n1 land n2)
      | Porint -> make_const_int (n1 lor n2)
      | Pxorint -> make_const_int (n1 lxor n2)
      | Plslint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_int (n1 lsl n2)
      | Plsrint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_int (n1 lsr n2)
      | Pasrint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_int (n1 asr n2)
      | Pintcomp c -> make_integer_comparison c n1 n2
      | _ -> default
      end
  (* float *)
  | [Value_const(Uconst_ref(_, Some (Uconst_float n1)))] when fpc ->
      begin match p with
      | Pintoffloat -> make_const_int (int_of_float n1)
      | Pnegfloat -> make_const_float (-. n1)
      | Pabsfloat -> make_const_float (abs_float n1)
      | _ -> default
      end
  (* float, float *)
  | [Value_const(Uconst_ref(_, Some (Uconst_float n1)));
     Value_const(Uconst_ref(_, Some (Uconst_float n2)))] when fpc ->
      begin match p with
      | Paddfloat -> make_const_float (n1 +. n2)
      | Psubfloat -> make_const_float (n1 -. n2)
      | Pmulfloat -> make_const_float (n1 *. n2)
      | Pdivfloat -> make_const_float (n1 /. n2)
      | Pfloatcomp c  -> make_float_comparison c n1 n2
      | _ -> default
      end
  (* nativeint *)
  | [Value_const(Uconst_ref(_, Some (Uconst_nativeint n)))] ->
      begin match p with
      | Pintofbint Pnativeint -> make_const_int (Nativeint.to_int n)
      | Pcvtbint(Pnativeint, Pint32) -> make_const_int32 (Nativeint.to_int32 n)
      | Pcvtbint(Pnativeint, Pint64) -> make_const_int64 (Int64.of_nativeint n)
      | Pnegbint Pnativeint -> make_const_natint (Nativeint.neg n)
      | _ -> default
      end
  (* nativeint, nativeint *)
  | [Value_const(Uconst_ref(_, Some (Uconst_nativeint n1)));
     Value_const(Uconst_ref(_, Some (Uconst_nativeint n2)))] ->
      begin match p with
      | Paddbint Pnativeint -> make_const_natint (Nativeint.add n1 n2)
      | Psubbint Pnativeint -> make_const_natint (Nativeint.sub n1 n2)
      | Pmulbint Pnativeint -> make_const_natint (Nativeint.mul n1 n2)
      | Pdivbint {size=Pnativeint} when n2 <> 0n ->
          make_const_natint (Nativeint.div n1 n2)
      | Pmodbint {size=Pnativeint} when n2 <> 0n ->
          make_const_natint (Nativeint.rem n1 n2)
      | Pandbint Pnativeint -> make_const_natint (Nativeint.logand n1 n2)
      | Porbint Pnativeint ->  make_const_natint (Nativeint.logor n1 n2)
      | Pxorbint Pnativeint -> make_const_natint (Nativeint.logxor n1 n2)
      | Pbintcomp(Pnativeint, c)  -> make_integer_comparison c n1 n2
      | _ -> default
      end
  (* nativeint, int *)
  | [Value_const(Uconst_ref(_, Some (Uconst_nativeint n1)));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pnativeint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_natint (Nativeint.shift_left n1 n2)
      | Plsrbint Pnativeint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_natint (Nativeint.shift_right_logical n1 n2)
      | Pasrbint Pnativeint when 0 <= n2 && n2 < 8 * B.size_int ->
          make_const_natint (Nativeint.shift_right n1 n2)
      | _ -> default
      end
  (* int32 *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int32 n)))] ->
      begin match p with
      | Pintofbint Pint32 -> make_const_int (Int32.to_int n)
      | Pcvtbint(Pint32, Pnativeint) -> make_const_natint (Nativeint.of_int32 n)
      | Pcvtbint(Pint32, Pint64) -> make_const_int64 (Int64.of_int32 n)
      | Pnegbint Pint32 -> make_const_int32 (Int32.neg n)
      | _ -> default
      end
  (* int32, int32 *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int32 n1)));
     Value_const(Uconst_ref(_, Some (Uconst_int32 n2)))] ->
      begin match p with
      | Paddbint Pint32 -> make_const_int32 (Int32.add n1 n2)
      | Psubbint Pint32 -> make_const_int32 (Int32.sub n1 n2)
      | Pmulbint Pint32 -> make_const_int32 (Int32.mul n1 n2)
      | Pdivbint {size=Pint32} when n2 <> 0l ->
          make_const_int32 (Int32.div n1 n2)
      | Pmodbint {size=Pint32} when n2 <> 0l ->
          make_const_int32 (Int32.rem n1 n2)
      | Pandbint Pint32 -> make_const_int32 (Int32.logand n1 n2)
      | Porbint Pint32 -> make_const_int32 (Int32.logor n1 n2)
      | Pxorbint Pint32 -> make_const_int32 (Int32.logxor n1 n2)
      | Pbintcomp(Pint32, c) -> make_integer_comparison c n1 n2
      | _ -> default
      end
  (* int32, int *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int32 n1)));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_left n1 n2)
      | Plsrbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_right_logical n1 n2)
      | Pasrbint Pint32 when 0 <= n2 && n2 < 32 ->
          make_const_int32 (Int32.shift_right n1 n2)
      | _ -> default
      end
  (* int64 *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int64 n)))] ->
      begin match p with
      | Pintofbint Pint64 -> make_const_int (Int64.to_int n)
      | Pcvtbint(Pint64, Pint32) -> make_const_int32 (Int64.to_int32 n)
      | Pcvtbint(Pint64, Pnativeint) -> make_const_natint (Int64.to_nativeint n)
      | Pnegbint Pint64 -> make_const_int64 (Int64.neg n)
      | _ -> default
      end
  (* int64, int64 *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int64 n1)));
     Value_const(Uconst_ref(_, Some (Uconst_int64 n2)))] ->
      begin match p with
      | Paddbint Pint64 -> make_const_int64 (Int64.add n1 n2)
      | Psubbint Pint64 -> make_const_int64 (Int64.sub n1 n2)
      | Pmulbint Pint64 -> make_const_int64 (Int64.mul n1 n2)
      | Pdivbint {size=Pint64} when n2 <> 0L ->
          make_const_int64 (Int64.div n1 n2)
      | Pmodbint {size=Pint64} when n2 <> 0L ->
          make_const_int64 (Int64.rem n1 n2)
      | Pandbint Pint64 -> make_const_int64 (Int64.logand n1 n2)
      | Porbint Pint64 -> make_const_int64 (Int64.logor n1 n2)
      | Pxorbint Pint64 -> make_const_int64 (Int64.logxor n1 n2)
      | Pbintcomp(Pint64, c) -> make_integer_comparison c n1 n2
      | _ -> default
      end
  (* int64, int *)
  | [Value_const(Uconst_ref(_, Some (Uconst_int64 n1)));
     Value_const(Uconst_int n2)] ->
      begin match p with
      | Plslbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_left n1 n2)
      | Plsrbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_right_logical n1 n2)
      | Pasrbint Pint64 when 0 <= n2 && n2 < 64 ->
          make_const_int64 (Int64.shift_right n1 n2)
      | _ -> default
      end
  (* TODO: Pbbswap *)
  (* Catch-all *)
  | _ ->
     default

let field_approx n = function
  | Value_tuple a when n < Array.length a -> a.(n)
  | Value_const (Uconst_ref(_, Some (Uconst_block(_, l))))
    when n < List.length l ->
      Value_const (List.nth l n)
  | _ -> Value_unknown

let simplif_prim_pure ~backend fpc p (args, approxs) dbg =
  let open Clambda_primitives in
  match p, args, approxs with
  (* Block construction *)
  | Pmakeblock(tag, Immutable, _kind), _, _ ->
      let field = function
        | Value_const c -> c
        | _ -> raise Exit
      in
      begin try
        let cst = Uconst_block (tag, List.map field approxs) in
        let name =
          Compilenv.new_structured_constant cst ~shared:true
        in
        make_const (Uconst_ref (name, Some cst))
      with Exit ->
        (Uprim(p, args, dbg), Value_tuple (Array.of_list approxs))
      end
  (* Field access *)
  | Pfield (n, _, _), _,
            [ Value_const(Uconst_ref(_, Some (Uconst_block(_, l)))) ]
    when n < List.length l ->
      make_const (List.nth l n)
  | Pfield(n, _, _), [ Uprim(P.Pmakeblock _, ul, _) ], [approx]
    when n < List.length ul ->
      (* This case is particularly useful for removing allocations
         for optional parameters *)
      (List.nth ul n, field_approx n approx)
  (* Strings *)
  | (Pstringlength | Pbyteslength),
     _,
     [ Value_const(Uconst_ref(_, Some (Uconst_string s))) ] ->
      make_const_int (String.length s)
  (* Kind test *)
  | Pisint, [ Uprim(P.Pmakeblock _, _, _) ], _ ->
      (* This case is particularly useful for removing allocations
         for optional parameters *)
      make_const_bool false
  | Pisint, _, [a1] ->
      begin match a1 with
      | Value_const(Uconst_int _) -> make_const_bool true
      | Value_const(Uconst_ref _) -> make_const_bool false
      | Value_closure _ | Value_tuple _ -> make_const_bool false
      | _ -> (Uprim(p, args, dbg), Value_unknown)
      end
  (* Catch-all *)
  | _ ->
      simplif_arith_prim_pure ~backend fpc p (args, approxs) dbg

let simplif_prim ~backend fpc p (args, approxs as args_approxs) dbg =
  if List.for_all is_pure args
  then simplif_prim_pure ~backend fpc p args_approxs dbg
  else
    (* XXX : always return the same approxs as simplif_prim_pure? *)
    let approx =
      match p with
      | P.Pmakeblock(_, Immutable, _kind) ->
          Value_tuple (Array.of_list approxs)
      | _ ->
          Value_unknown
    in
    (Uprim(p, args, dbg), approx)

(* Substitute variables in a [ulambda] term (a body of an inlined function)
   and perform some more simplifications on integer primitives.
   Also perform alpha-conversion on let-bound identifiers to avoid
   clashes with locally-generated identifiers, and refresh raise counts
   in order to avoid clashes with inlined code from other modules.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion, and also for the translation of let rec
   over functions. *)

let approx_ulam = function
    Uconst c -> Value_const c
  | _ -> Value_unknown

let find_action idxs acts tag =
  if 0 <= tag && tag < Array.length idxs then begin
    let idx = idxs.(tag) in
    assert(0 <= idx && idx < Array.length acts);
    Some acts.(idx)
  end else
    (* Can this happen? *)
    None

let subst_debuginfo loc dbg =
  if !Clflags.debug then
    Debuginfo.inline loc dbg
  else
    dbg

let rec substitute loc ((backend, fpc) as st) sb rn ulam =
  match ulam with
    Uvar v ->
      begin try V.Map.find v sb with Not_found -> ulam end
  | Uconst _ -> ulam
  | Udirect_apply(lbl, args, dbg) ->
      let dbg = subst_debuginfo loc dbg in
      Udirect_apply(lbl, List.map (substitute loc st sb rn) args, dbg)
  | Ugeneric_apply(fn, args, dbg) ->
      let dbg = subst_debuginfo loc dbg in
      Ugeneric_apply(substitute loc st sb rn fn,
                     List.map (substitute loc st sb rn) args, dbg)
  | Uclosure(defs, env) ->
      (* Question: should we rename function labels as well?  Otherwise,
         there is a risk that function labels are not globally unique.
         This should not happen in the current system because:
         - Inlined function bodies contain no Uclosure nodes
           (cf. function [lambda_smaller])
         - When we substitute offsets for idents bound by let rec
           in [close], case [Lletrec], we discard the original
           let rec body and use only the substituted term. *)
      Uclosure(defs, List.map (substitute loc st sb rn) env)
  | Uoffset(u, ofs) -> Uoffset(substitute loc st sb rn u, ofs)
  | Ulet(str, kind, id, u1, u2) ->
      let id' = VP.rename id in
      Ulet(str, kind, id', substitute loc st sb rn u1,
           substitute loc st
             (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u2)
  | Uphantom_let _ -> no_phantom_lets ()
  | Uprim(p, args, dbg) ->
      let sargs = List.map (substitute loc st sb rn) args in
      let dbg = subst_debuginfo loc dbg in
      let (res, _) =
        simplif_prim ~backend fpc p (sargs, List.map approx_ulam sargs) dbg in
      res
  | Uswitch(arg, sw, dbg) ->
      let sarg = substitute loc st sb rn arg in
      let action =
        (* Unfortunately, we cannot easily deal with the
           case of a constructed block (makeblock) bound to a local
           identifier.  This would require to keep track of
           local let bindings (at least their approximations)
           in this substitute function.
        *)
        match sarg with
        | Uconst (Uconst_ref (_,  Some (Uconst_block (tag, _)))) ->
            find_action sw.us_index_blocks sw.us_actions_blocks tag
        | Uconst (Uconst_int tag) ->
            find_action sw.us_index_consts sw.us_actions_consts tag
        | _ -> None
      in
      begin match action with
      | Some u -> substitute loc st sb rn u
      | None ->
          Uswitch(sarg,
                  { sw with
                    us_actions_consts =
                      Array.map (substitute loc st sb rn) sw.us_actions_consts;
                    us_actions_blocks =
                      Array.map (substitute loc st sb rn) sw.us_actions_blocks;
                  },
                  dbg)
      end
  | Ustringswitch(arg,sw,d) ->
      Ustringswitch
        (substitute loc st sb rn arg,
         List.map (fun (s,act) -> s,substitute loc st sb rn act) sw,
         Option.map (substitute loc st sb rn) d)
  | Ustaticfail (nfail, args) ->
      let nfail =
        match rn with
        | Some rn ->
          begin try
            Int.Map.find nfail rn
          with Not_found ->
            fatal_errorf "Closure.split_list: invalid nfail (%d)" nfail
          end
        | None -> nfail in
      Ustaticfail (nfail, List.map (substitute loc st sb rn) args)
  | Ucatch(nfail, ids, u1, u2) ->
      let nfail, rn =
        match rn with
        | Some rn ->
          let new_nfail = next_raise_count () in
          new_nfail, Some (Int.Map.add nfail new_nfail rn)
        | None -> nfail, rn in
      let ids' = List.map (fun (id, k) -> VP.rename id, k) ids in
      let sb' =
        List.fold_right2
          (fun (id, _) (id', _) s ->
             V.Map.add (VP.var id) (Uvar (VP.var id')) s
          )
          ids ids' sb
      in
      Ucatch(nfail, ids', substitute loc st sb rn u1,
                          substitute loc st sb' rn u2)
  | Utrywith(u1, id, u2) ->
      let id' = VP.rename id in
      Utrywith(substitute loc st sb rn u1, id',
               substitute loc st
                 (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u2)
  | Uifthenelse(u1, u2, u3) ->
      begin match substitute loc st sb rn u1 with
        Uconst (Uconst_int n) ->
          if n <> 0 then
            substitute loc st sb rn u2
          else
            substitute loc st sb rn u3
      | su1 ->
          Uifthenelse(su1, substitute loc st sb rn u2,
                           substitute loc st sb rn u3)
      end
  | Usequence(u1, u2) ->
      Usequence(substitute loc st sb rn u1, substitute loc st sb rn u2)
  | Uwhile(u1, u2) ->
      Uwhile(substitute loc st sb rn u1, substitute loc st sb rn u2)
  | Ufor(id, u1, u2, dir, u3) ->
      let id' = VP.rename id in
      Ufor(id', substitute loc st sb rn u1, substitute loc st sb rn u2, dir,
           substitute loc st
           (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u3)
  | Uassign(id, u) ->
      let id' =
        try
          match V.Map.find id sb with Uvar i -> i | _ -> assert false
        with Not_found ->
          id in
      Uassign(id', substitute loc st sb rn u)
  | Usend(k, u1, u2, ul, dbg) ->
      let dbg = subst_debuginfo loc dbg in
      Usend(k, substitute loc st sb rn u1, substitute loc st sb rn u2,
            List.map (substitute loc st sb rn) ul, dbg)
  | Uunreachable ->
      Uunreachable

type closure_entry =
  | Free_variable of int
  | Function of int

type closure_env =
  | Not_in_closure
  | In_closure of {
      entries: closure_entry V.Map.t;
      env_param: V.t;
      env_pos: int;
    }

type env = {
  backend : (module Backend_intf.S);
  cenv : closure_env;
  fenv : value_approximation V.Map.t;
  mutable_vars : V.Set.t;
}

(* Perform an inline expansion:

   If [f p = body], substitute [f a] by [let p = a in body].

   Under certain conditions, further simplifications are possible (we use the
   terminology of [Semantics_of_primitives], applied to terms of the Clambda
   language):

   - [f a] is equivalent to [body[a/p]] if [a] has no effects and no coeffects.
     However, we only want to do this rewriting if [body[a/p]] does not increase
     the size of [body]. Since this is hard to decide in general, as an
     approximation, only consider the case when [a] is an immutable variable or
     a constant.

   - [f a] is equivalent to [body] if [p] does not occur in [body] and [a] has
     only generative effects.

   - In general [f a] is equivalent to [a; body] if [p] does not occur in
     [body].
*)

(* Approximates "no effects and no coeffects" *)
let rec is_substituable ~mutable_vars = function
  | Uvar v -> not (V.Set.mem v mutable_vars)
  | Uconst _ -> true
  | Uoffset(arg, _) -> is_substituable ~mutable_vars arg
  | _ -> false

(* Approximates "only generative effects" *)
let is_erasable = function
  | Uclosure _ -> true
  | u -> is_pure u

let bind_params { backend; mutable_vars; _ } loc fdesc params args funct body =
  let fpc = fdesc.fun_float_const_prop in
  let rec aux subst pl al body =
    match (pl, al) with
      ([], []) -> substitute (Debuginfo.from_location loc) (backend, fpc)
                    subst (Some Int.Map.empty) body
    | (p1 :: pl, a1 :: al) ->
        if is_substituable ~mutable_vars a1 then
          aux (V.Map.add (VP.var p1) a1 subst) pl al body
        else begin
          let p1' = VP.rename p1 in
          let u1, u2 =
            match VP.name p1, a1 with
            | "*opt*", Uprim(P.Pmakeblock(0, Immutable, kind), [a], dbg) ->
                (* This parameter corresponds to an optional parameter,
                   and although it is used twice pushing the expression down
                   actually allows us to remove the allocation as it will
                   appear once under a Pisint primitive and once under a Pfield
                   primitive (see [simplif_prim_pure]) *)
                a, Uprim(P.Pmakeblock(0, Immutable, kind),
                         [Uvar (VP.var p1')], dbg)
            | _ ->
                a1, Uvar (VP.var p1')
          in
          let body' = aux (V.Map.add (VP.var p1) u2 subst) pl al body in
          if occurs_var (VP.var p1) body then
            Ulet(Immutable, Pgenval, p1', u1, body')
          else if is_erasable a1 then body'
          else Usequence(a1, body')
        end
    | (_, _) -> assert false
  in
  (* Reverse parameters and arguments to preserve right-to-left
     evaluation order (PR#2910). *)
  let params, args = List.rev params, List.rev args in
  let params, args, body =
    (* Ensure funct is evaluated after args *)
    match params with
    | my_closure :: params when not fdesc.fun_closed ->
       (params @ [my_closure]), (args @ [funct]), body
    | _ ->
       params, args, (if is_pure funct then body else Usequence (funct, body))
  in
  aux V.Map.empty params args body

let warning_if_forced_inline ~loc ~attribute warning =
  if attribute = Always_inline then
    Location.prerr_warning (Debuginfo.Scoped_location.to_location loc)
      (Warnings.Inlining_impossible warning)

(* Generate a direct application *)

let direct_apply env fundesc ufunct uargs ~loc ~attribute =
  match fundesc.fun_inline, attribute with
  | _, Never_inline
  | None, _ ->
     let dbg = Debuginfo.from_location loc in
     warning_if_forced_inline ~loc ~attribute
       "Function information unavailable";
     if fundesc.fun_closed && is_pure ufunct then
       Udirect_apply(fundesc.fun_label, uargs, dbg)
     else if not fundesc.fun_closed &&
               is_substituable ~mutable_vars:env.mutable_vars ufunct then
       Udirect_apply(fundesc.fun_label, uargs @ [ufunct], dbg)
     else begin
       let args = List.map (fun arg ->
         if is_substituable ~mutable_vars:env.mutable_vars arg then
           None, arg
         else
           let id = V.create_local "arg" in
           Some (VP.create id, arg), Uvar id) uargs in
       let app_args = List.map snd args in
       List.fold_left (fun app (binding,_) ->
           match binding with
           | None -> app
           | Some (v, e) -> Ulet(Immutable, Pgenval, v, e, app))
         (if fundesc.fun_closed then
            Usequence (ufunct, Udirect_apply (fundesc.fun_label, app_args, dbg))
          else
            let clos = V.create_local "clos" in
            Ulet(Immutable, Pgenval, VP.create clos, ufunct,
                 Udirect_apply(fundesc.fun_label, app_args @ [Uvar clos], dbg)))
         args
       end
  | Some(params, body), _  ->
     bind_params env loc fundesc params uargs ufunct body

(* Add [Value_integer] info to the approximation of an application *)

let strengthen_approx appl approx =
  match approx_ulam appl with
    (Value_const _) as intapprox ->
      intapprox
  | _ -> approx

(* If a term has approximation Value_integer and is pure,
   replace it by an integer constant *)

let check_constant_result ulam approx =
  match approx with
    Value_const c when is_pure ulam -> make_const c
  | Value_global_field (id, i) when is_pure ulam ->
      begin match ulam with
      | Uprim(P.Pfield _, [Uprim(P.Pread_symbol _, _, _)], _) -> (ulam, approx)
      | _ ->
          let glb =
            Uprim(P.Pread_symbol id, [], Debuginfo.none)
          in
          Uprim(P.Pfield(i, Pointer, Immutable), [glb], Debuginfo.none), approx
      end
  | _ -> (ulam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr ulam1 (ulam2, approx2 as res2) =
  if is_pure ulam1 then res2 else (Usequence(ulam1, ulam2), approx2)

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Maintain the nesting depth for functions *)

let function_nesting_depth = ref 0
let excessive_function_nesting_depth = 5

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

exception NotClosed

let close_approx_var { fenv; cenv } id =
  let approx = try V.Map.find id fenv with Not_found -> Value_unknown in
  match approx with
    Value_const c -> make_const c
  | approx ->
      match cenv with
      | Not_in_closure -> Uvar id, approx
      | In_closure { entries; env_param; env_pos } ->
        let subst =
          match V.Map.find id entries with
          | Free_variable fv_pos ->
            Uprim(P.Pfield(fv_pos - env_pos, Pointer, Immutable),
                  [Uvar env_param], Debuginfo.none)
          | Function fun_pos ->
            Uoffset(Uvar env_param, fun_pos - env_pos)
          | exception Not_found -> Uvar id
        in
        (subst, approx)

let close_var env id =
  let (ulam, _app) = close_approx_var env id in ulam

let rec close ({ backend; fenv; cenv ; mutable_vars } as env) lam =
  let module B = (val backend : Backend_intf.S) in
  match lam with
  | Lvar id ->
     close_approx_var env id
  | Lmutvar id -> (Uvar id, Value_unknown)
  | Lconst cst ->
      let str cst =
        let name =
          Compilenv.new_structured_constant cst ~shared:true
        in
        Uconst_ref (name, Some cst)
      in
      let rec transl = function
        | Const_base(Const_int n) -> Uconst_int n
        | Const_base(Const_char c) -> Uconst_int (Char.code c)
        | Const_block (tag, fields) ->
            str (Uconst_block (tag, List.map transl fields))
        | Const_float_array sl ->
            (* constant float arrays are really immutable *)
            str (Uconst_float_array (List.map float_of_string sl))
        | Const_immstring s ->
            str (Uconst_string s)
        | Const_base (Const_string (s, _, _)) ->
            str (Uconst_string s)
        | Const_base(Const_float x) -> str (Uconst_float (float_of_string x))
        | Const_base(Const_int32 x) -> str (Uconst_int32 x)
        | Const_base(Const_int64 x) -> str (Uconst_int64 x)
        | Const_base(Const_nativeint x) -> str (Uconst_nativeint x)
      in
      make_const (transl cst)
  | Lfunction funct ->
      close_one_function env (Ident.create_local "fun") funct

    (* We convert [f a] to [let a' = a in let f' = f in fun b c -> f' a' b c]
       when fun_arity > nargs *)
  | Lapply{ap_func = funct; ap_args = args; ap_loc = loc;
        ap_inlined = attribute} ->
      let nargs = List.length args in
      begin match (close env funct, close_list env args) with
        ((ufunct, Value_closure(fundesc, approx_res)),
         [Uprim(P.Pmakeblock _, uargs, _)])
        when List.length uargs = - fundesc.fun_arity ->
          let app =
            direct_apply env ~loc ~attribute fundesc ufunct uargs in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when nargs = fundesc.fun_arity ->
          let app =
            direct_apply env ~loc ~attribute fundesc ufunct uargs in
          (app, strengthen_approx app approx_res)

      | ((ufunct, (Value_closure(fundesc, _) as fapprox)), uargs)
          when nargs < fundesc.fun_arity ->
        let first_args = List.map (fun arg ->
          (V.create_local "arg", arg) ) uargs in
        let final_args =
          Array.to_list (Array.init (fundesc.fun_arity - nargs)
                                    (fun _ -> V.create_local "arg")) in
        let rec iter args body =
          match args with
              [] -> body
            | (arg1, arg2) :: args ->
              iter args
                (Ulet (Immutable, Pgenval, VP.create arg1, arg2, body))
        in
        let internal_args =
          (List.map (fun (arg1, _arg2) -> Lvar arg1) first_args)
          @ (List.map (fun arg -> Lvar arg ) final_args)
        in
        let funct_var = V.create_local "funct" in
        let fenv = V.Map.add funct_var fapprox fenv in
        let (new_fun, approx) = close { backend; fenv; cenv; mutable_vars }
          (lfunction
             ~kind:Curried
             ~return:Pgenval
             ~params:(List.map (fun v -> v, Pgenval) final_args)
             ~body:(Lapply{
                ap_loc=loc;
                ap_func=(Lvar funct_var);
                ap_args=internal_args;
                ap_tailcall=Default_tailcall;
                ap_inlined=Default_inline;
                ap_specialised=Default_specialise;
              })
             ~loc
             ~attr:default_function_attribute)
        in
        let new_fun =
          iter first_args
            (Ulet (Immutable, Pgenval, VP.create funct_var, ufunct, new_fun))
        in
        warning_if_forced_inline ~loc ~attribute "Partial application";
        (new_fun, approx)

      | ((ufunct, Value_closure(fundesc, _approx_res)), uargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let args = List.map (fun arg -> V.create_local "arg", arg) uargs in
          let (first_args, rem_args) = split_list fundesc.fun_arity args in
          let first_args = List.map (fun (id, _) -> Uvar id) first_args in
          let rem_args = List.map (fun (id, _) -> Uvar id) rem_args in
          let dbg = Debuginfo.from_location loc in
          warning_if_forced_inline ~loc ~attribute "Over-application";
          let body =
            Ugeneric_apply(direct_apply env ~loc ~attribute
                              fundesc ufunct first_args,
                           rem_args, dbg)
          in
          let result =
            List.fold_left (fun body (id, defining_expr) ->
                Ulet (Immutable, Pgenval, VP.create id, defining_expr, body))
              body
              args
          in
          result, Value_unknown
      | ((ufunct, _), uargs) ->
          let dbg = Debuginfo.from_location loc in
          warning_if_forced_inline ~loc ~attribute "Unknown function";
          (Ugeneric_apply(ufunct, uargs, dbg), Value_unknown)
      end
  | Lsend(kind, met, obj, args, loc) ->
      let (umet, _) = close env met in
      let (uobj, _) = close env obj in
      let dbg = Debuginfo.from_location loc in
      (Usend(kind, umet, uobj, close_list env args, dbg),
       Value_unknown)
  | Llet(str, kind, id, lam, body) ->
      let (ulam, alam) = close_named env id lam in
      begin match alam with
        Value_const _
           when str = Alias || is_pure ulam ->
         close { backend; fenv = (V.Map.add id alam fenv); cenv; mutable_vars }
           body
      | _ ->
         let (ubody, abody) =
           close
             { backend; fenv = (V.Map.add id alam fenv); cenv; mutable_vars }
             body
         in
         (Ulet(Immutable, kind, VP.create id, ulam, ubody), abody)
      end
  | Lmutlet(kind, id, lam, body) ->
     let (ulam, _) = close_named env id lam in
     let env = {env with mutable_vars = V.Set.add id env.mutable_vars} in
     let (ubody, abody) = close env body in
     (Ulet(Mutable, kind, VP.create id, ulam, ubody), abody)
  | Lletrec(defs, body) ->
      let (clos, infos) = close_functions env defs in
      let clos_ident = V.create_local "clos" in
      let fenv_body =
        List.fold_right
          (fun (id, _pos, approx) fenv -> V.Map.add id approx fenv)
          infos fenv in
      let (ubody, approx) =
        close { backend; fenv = fenv_body; cenv; mutable_vars } body in
      let sb =
        List.fold_right
          (fun (id, pos, _approx) sb ->
             V.Map.add id (Uoffset(Uvar clos_ident, pos)) sb)
          infos V.Map.empty in
      (Ulet(Immutable, Pgenval, VP.create clos_ident, clos,
            substitute Debuginfo.none (backend, !Clflags.float_const_prop) sb
              None ubody),
       approx)
  (* Compile-time constants *)
  | Lprim(Pctconst c, [arg], _loc) ->
      let cst, approx =
        match c with
        | Big_endian -> make_const_bool B.big_endian
        | Word_size -> make_const_int (8*B.size_int)
        | Int_size -> make_const_int (8*B.size_int - 1)
        | Max_wosize -> make_const_int ((1 lsl ((8*B.size_int) - 10)) - 1 )
        | Ostype_unix -> make_const_bool (Config.target_os_type = "Unix")
        | Ostype_win32 -> make_const_bool (Config.target_os_type = "Win32")
        | Ostype_cygwin -> make_const_bool (Config.target_os_type = "Cygwin")
        | Backend_type ->
            make_const_int 0 (* tag 0 is the same as Native here *)
      in
      let arg, _approx = close env arg in
      let id = Ident.create_local "dummy" in
      Ulet(Immutable, Pgenval, VP.create id, arg, cst), approx
  | Lprim(Pignore, [arg], _loc) ->
      let expr, approx = make_const_int 0 in
      Usequence(fst (close env arg), expr), approx
  | Lprim((Pbytes_to_string | Pbytes_of_string), [arg], _loc) ->
      close env arg
  | Lprim(Pgetglobal id, [], loc) ->
      let dbg = Debuginfo.from_location loc in
      check_constant_result (getglobal dbg id)
                            (Compilenv.global_approx id)
  | Lprim(Pfield (n, ptr, mut), [lam], loc) ->
      let (ulam, approx) = close env lam in
      let dbg = Debuginfo.from_location loc in
      check_constant_result (Uprim(P.Pfield (n, ptr, mut), [ulam], dbg))
                            (field_approx n approx)
  | Lprim(Psetfield(n, is_ptr, init), [Lprim(Pgetglobal id, [], _); lam], loc)->
      let (ulam, approx) = close env lam in
      if approx <> Value_unknown then
        (!global_approx).(n) <- approx;
      let dbg = Debuginfo.from_location loc in
      (Uprim(P.Psetfield(n, is_ptr, init), [getglobal dbg id; ulam], dbg),
       Value_unknown)
  | Lprim(Praise k, [arg], loc) ->
      let (ulam, _approx) = close env arg in
      let dbg = Debuginfo.from_location loc in
      (Uprim(P.Praise k, [ulam], dbg),
       Value_unknown)
  | Lprim (Pmakearray _, [], _loc) -> make_const_ref (Uconst_block (0, []))
  | Lprim(p, args, loc) ->
      let p = Convert_primitives.convert p in
      let dbg = Debuginfo.from_location loc in
      simplif_prim ~backend !Clflags.float_const_prop
                   p (close_list_approx env args) dbg
  | Lswitch(arg, sw, dbg) ->
      let fn fail =
        let (uarg, _) = close env arg in
        let const_index, const_actions, fconst =
          close_switch env sw.sw_consts sw.sw_numconsts fail
        and block_index, block_actions, fblock =
          close_switch env sw.sw_blocks sw.sw_numblocks fail in
        let ulam =
          Uswitch
            (uarg,
             {us_index_consts = const_index;
              us_actions_consts = const_actions;
              us_index_blocks = block_index;
              us_actions_blocks = block_actions},
             Debuginfo.from_location dbg)
        in
        (fconst (fblock ulam),Value_unknown) in
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let fail = sw.sw_failaction in
      begin match fail with
      | None|Some (Lstaticraise (_,_)) -> fn fail
      | Some lamfail ->
          if
            (sw.sw_numconsts - List.length sw.sw_consts) +
            (sw.sw_numblocks - List.length sw.sw_blocks) > 1
          then
            let i = next_raise_count () in
            let ubody,_ = fn (Some (Lstaticraise (i,[])))
            and uhandler,_ = close env lamfail in
            Ucatch (i,[],ubody,uhandler),Value_unknown
          else fn fail
      end
  | Lstringswitch(arg,sw,d,_) ->
      let uarg,_ = close env arg in
      let usw =
        List.map
          (fun (s,act) ->
            let uact,_ = close env act in
            s,uact)
          sw in
      let ud =
        Option.map
          (fun d ->
            let ud,_ = close env d in
            ud) d in
      Ustringswitch (uarg,usw,ud),Value_unknown
  | Lstaticraise (i, args) ->
      (Ustaticfail (i, close_list env args), Value_unknown)
  | Lstaticcatch(body, (i, vars), handler) ->
      let (ubody, _) = close env body in
      let (uhandler, _) = close env handler in
      let vars = List.map (fun (var, k) -> VP.create var, k) vars in
      (Ucatch(i, vars, ubody, uhandler), Value_unknown)
  | Ltrywith(body, id, handler) ->
      let (ubody, _) = close env body in
      let (uhandler, _) = close env handler in
      (Utrywith(ubody, VP.create id, uhandler), Value_unknown)
  | Lifthenelse(arg, ifso, ifnot) ->
      begin match close env arg with
        (uarg, Value_const (Uconst_int n)) ->
          sequence_constant_expr uarg
            (close env (if n = 0 then ifnot else ifso))
      | (uarg, _ ) ->
          let (uifso, _) = close env ifso in
          let (uifnot, _) = close env ifnot in
          (Uifthenelse(uarg, uifso, uifnot), Value_unknown)
      end
  | Lsequence(lam1, lam2) ->
      let (ulam1, _) = close env lam1 in
      let (ulam2, approx) = close env lam2 in
      (Usequence(ulam1, ulam2), approx)
  | Lwhile(cond, body) ->
      let (ucond, _) = close env cond in
      let (ubody, _) = close env body in
      (Uwhile(ucond, ubody), Value_unknown)
  | Lfor(id, lo, hi, dir, body) ->
      let (ulo, _) = close env lo in
      let (uhi, _) = close env hi in
      let (ubody, _) = close env body in
      (Ufor(VP.create id, ulo, uhi, dir, ubody), Value_unknown)
  | Lassign(id, lam) ->
      let (ulam, _) = close env lam in
      (Uassign(id, ulam), Value_unknown)
  | Levent(lam, _) ->
      close env lam
  | Lifused _ ->
      assert false

and close_list env = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close env lam in
      ulam :: close_list env rem

and close_list_approx env = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close env lam in
      let (ulams, approxs) = close_list_approx env rem in
      (ulam :: ulams, approx :: approxs)

and close_named env id = function
    Lfunction funct ->
      close_one_function env id funct
  | lam ->
      close env lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions { backend; fenv; cenv; mutable_vars } fun_defs =
  let fun_defs =
    (* Split functions with optional arguments and default values into
       a wrapper function (likely to be inlined) and an inner function
       (never inlined).

       However, if the user forces inlining of the function, this is
       counterproductive; we want the whole function to be inlined,
       not just the wrapper. So we disable the split when inlining
       is forced. Cf #12526
    *)
    match fun_defs with
    | [{ def = {attr = { inline = Always_inline; }}}] ->
        fun_defs
    | _ ->
        List.concat_map
          (function
           | { id;
               def = {kind; params; return; body; attr; loc} } ->
             Simplif.split_default_wrapper ~id ~kind ~params
               ~body ~attr ~loc ~return
         )
         fun_defs
  in
  let inline_attribute = match fun_defs with
    | [{ def = {attr = { inline; }}}] -> inline
    | _ -> Default_inline (* recursive functions can't be inlined *)
  in
  (* Update and check nesting depth *)
  incr function_nesting_depth;
  let initially_closed =
    !function_nesting_depth < excessive_function_nesting_depth in
  (* Determine the free variables of the functions *)
  let fv =
    V.Set.elements (free_variables (Lletrec(fun_defs, lambda_unit))) in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          { id;
            def = {kind; params; return; body; loc; attr} } ->
            let label = Compilenv.make_symbol (Some (V.unique_name id)) in
            let arity = List.length params in
            let fundesc =
              {fun_label = label;
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_closed = initially_closed;
               fun_inline = None;
               fun_float_const_prop = !Clflags.float_const_prop;
               fun_poll = attr.poll } in
            let dbg = Debuginfo.from_location loc in
            (id, params, return, body, fundesc, dbg))
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun (id, _params, _return, _body, fundesc, _dbg) fenv ->
        V.Map.add id (Value_closure(fundesc, Value_unknown)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (_id, _params, _return, _body, fundesc, _dbg) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  let cenv_entries =
    let rec free_variables_entries fv_pos = function
        [] -> V.Map.empty
      | id :: rem ->
          V.Map.add id (Free_variable fv_pos)
            (free_variables_entries (fv_pos+1) rem)
    in
    let entries_fv = free_variables_entries fv_pos fv in
    List.fold_right2
      (fun (id, _params, _return, _body, _fundesc, _dbg) pos env ->
         V.Map.add id (Function pos) env)
      uncurried_defs clos_offsets entries_fv
  in
  (* Translate each function definition *)
  let clos_fundef (id, params, return, body, fundesc, dbg) env_pos =
    let env_param = V.create_local "env" in
    let cenv_body =
      In_closure {
        entries = cenv_entries;
        env_param;
        env_pos;
      }
    in
    let (ubody, approx) =
      close { backend; fenv = fenv_rec; cenv = cenv_body; mutable_vars } body
    in
    if !useless_env && occurs_var env_param ubody then raise NotClosed;
    let fun_params =
      if !useless_env
      then params
      else params @ [env_param, Pgenval]
    in
    let f =
      {
        label  = fundesc.fun_label;
        arity  = fundesc.fun_arity;
        params = List.map (fun (var, kind) -> VP.create var, kind) fun_params;
        return;
        body   = ubody;
        dbg;
        env = Some env_param;
        poll = fundesc.fun_poll
      }
    in
    (* give more chance of function with default parameters (i.e.
       their wrapper functions) to be inlined *)
    let n =
      List.fold_left
        (fun n (id, _) -> n + if V.name id = "*opt*" then 8 else 1)
        0
        fun_params
    in
    let threshold =
      match inline_attribute with
      | Default_inline ->
          let inline_threshold =
            Clflags.Float_arg_helper.get ~key:0 !Clflags.inline_threshold
          in
          let magic_scale_constant = 8. in
          int_of_float (inline_threshold *. magic_scale_constant) + n
      | Always_inline | Hint_inline -> max_int
      | Never_inline -> min_int
      | Unroll _ -> assert false
    in
    let fun_params = List.map (fun (var, _) -> VP.create var) fun_params in
    if lambda_smaller ubody threshold
    then fundesc.fun_inline <- Some(fun_params, ubody);

    (f, (id, env_pos, Value_closure(fundesc, approx))) in
  (* Translate all function definitions. *)
  let clos_info_list =
    if initially_closed then begin
      let snap = Compilenv.snapshot () in
      try List.map2 clos_fundef uncurried_defs clos_offsets
      with NotClosed ->
      (* If the hypothesis that the environment parameters are useless has been
         invalidated, then set [fun_closed] to false in all descriptions and
         recompile *)
        Compilenv.backtrack snap; (* PR#6337 *)
        List.iter
          (fun (_id, _params, _return, _body, fundesc, _dbg) ->
             fundesc.fun_closed <- false;
             fundesc.fun_inline <- None;
          )
          uncurried_defs;
        useless_env := false;
        List.map2 clos_fundef uncurried_defs clos_offsets
    end else
      (* Excessive closure nesting: assume environment parameter is used *)
        List.map2 clos_fundef uncurried_defs clos_offsets
    in
  (* Update nesting depth *)
  decr function_nesting_depth;
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in
  let fv = if !useless_env then [] else fv in
  (Uclosure(clos,
            List.map (close_var { backend; fenv; cenv; mutable_vars }) fv),
   infos)

(* Same, for one non-recursive function *)

and close_one_function env id funct =
  match close_functions env [{ id; def = funct }] with
  | (clos, (i, _, approx) :: _) when id = i -> (clos, approx)
  | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch env cases num_keys default =
  let ncases = List.length cases in
  let index = Array.make num_keys 0
  and store = Storer.mk_store () in

  (* First default case *)
  begin match default with
  | Some def when ncases < num_keys ->
      assert (store.act_store () def = 0)
  | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam) ->
     index.(key) <- store.act_store () lam)
    cases ;

  (*  Explicit sharing with catch/exit, as switcher compilation may
      later unshare *)
  let acts = store.act_get_shared () in
  let hs = ref (fun e -> e) in

  (* Compile actions *)
  let actions =
    Array.map
      (function
        | Single lam|Shared (Lstaticraise (_,[]) as lam) ->
            let ulam,_ = close env lam in
            ulam
        | Shared lam ->
            let ulam,_ = close env lam in
            let i = next_raise_count () in
(*
            let string_of_lambda e =
              Printlambda.lambda Format.str_formatter e ;
              Format.flush_str_formatter () in
            Printf.eprintf "SHARE CLOSURE %i [%s]\n%s\n" i
                (string_of_lambda arg)
                (string_of_lambda lam) ;
*)
            let ohs = !hs in
            hs := (fun e -> Ucatch (i,[],ohs e,ulam)) ;
            Ustaticfail (i,[]))
      acts in
  match actions with
  | [| |] -> [| |], [| |], !hs (* May happen when default is None *)
  | _     -> index, actions, !hs


(* Collect exported symbols for structured constants *)

let collect_exported_structured_constants a =
  let rec approx = function
    | Value_closure (fd, a) ->
        approx a;
        begin match fd.fun_inline with
        | Some (_, u) -> ulam u
        | None -> ()
        end
    | Value_tuple a -> Array.iter approx a
    | Value_const c -> const c
    | Value_unknown | Value_global_field _ -> ()
  and const = function
    | Uconst_ref (s, (Some c)) ->
        Compilenv.add_exported_constant s;
        structured_constant c
    | Uconst_ref (_s, None) -> assert false (* Cannot be generated *)
    | Uconst_int _ -> ()
  and structured_constant = function
    | Uconst_block (_, ul) -> List.iter const ul
    | Uconst_float _ | Uconst_int32 _
    | Uconst_int64 _ | Uconst_nativeint _
    | Uconst_float_array _ | Uconst_string _ -> ()
    | Uconst_closure _ -> assert false (* Cannot be generated *)
  and ulam = function
    | Uvar _ -> ()
    | Uconst c -> const c
    | Udirect_apply (_, ul, _) -> List.iter ulam ul
    | Ugeneric_apply (u, ul, _) -> ulam u; List.iter ulam ul
    | Uclosure (fl, ul) ->
        List.iter (fun f -> ulam f.body) fl;
        List.iter ulam ul
    | Uoffset(u, _) -> ulam u
    | Ulet (_str, _kind, _, u1, u2) -> ulam u1; ulam u2
    | Uphantom_let _ -> no_phantom_lets ()
    | Uprim (_, ul, _) -> List.iter ulam ul
    | Uswitch (u, sl, _dbg) ->
        ulam u;
        Array.iter ulam sl.us_actions_consts;
        Array.iter ulam sl.us_actions_blocks
    | Ustringswitch (u,sw,d) ->
        ulam u ;
        List.iter (fun (_,act) -> ulam act) sw ;
        Option.iter ulam d
    | Ustaticfail (_, ul) -> List.iter ulam ul
    | Ucatch (_, _, u1, u2)
    | Utrywith (u1, _, u2)
    | Usequence (u1, u2)
    | Uwhile (u1, u2)  -> ulam u1; ulam u2
    | Uifthenelse (u1, u2, u3)
    | Ufor (_, u1, u2, _, u3) -> ulam u1; ulam u2; ulam u3
    | Uassign (_, u) -> ulam u
    | Usend (_, u1, u2, ul, _) -> ulam u1; ulam u2; List.iter ulam ul
    | Uunreachable -> ()
  in
  approx a

let reset () =
  global_approx := [||];
  function_nesting_depth := 0

(* The entry point *)

let intro ~backend ~size lam =
  reset ();
  let id = Compilenv.make_symbol None in
  global_approx := Array.init size (fun i -> Value_global_field (id, i));
  Compilenv.set_global_approx(Value_tuple !global_approx);
  let (ulam, _approx) =
    close { backend; fenv = V.Map.empty;
            cenv = Not_in_closure; mutable_vars = V.Set.empty } lam
  in
  let opaque =
    !Clflags.opaque
    || Env.is_imported_opaque (Compilenv.current_unit_name ())
  in
  if opaque
  then Compilenv.set_global_approx(Value_unknown)
  else collect_exported_structured_constants (Value_tuple !global_approx);
  global_approx := [||];
  ulam
