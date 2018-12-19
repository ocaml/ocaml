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

module CB = Debuginfo.Current_block

module Int = Numbers.Int
module Storer =
  Switch.Store
    (struct
      type t = Location.t * lambda
      type key = lambda
      let make_key (_loc, lam) = Lambda.make_key lam
      let compare_key = Stdlib.compare
    end)

module V = Backend_var
module VP = Backend_var.With_provenance

(* Maintain the current module path *)

let current_module_path = ref (None : Path.t option)

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> V.Map.empty
  | id :: rem ->
      V.Map.add id
        (Uprim(Pfield pos, [Uvar env_param], Debuginfo.none))
          (build_closure_env env_param (pos+1) rem)

(* Auxiliary for accessing globals.  We change the name of the global
   to the name of the corresponding asm symbol.  This is done here
   and no longer in Cmmgen so that approximations stored in .cmx files
   contain the right names if the -for-pack option is active. *)

let getglobal dbg id =
  Uprim(Pgetglobal (V.create_persistent (Compilenv.symbol_for_global id)),
        [], dbg)

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
    | Uphantom_let (_var, _defining_expr, body) -> occurs body
    | Uletrec(decls, body) ->
        List.exists (fun (_id, u) -> occurs u) decls || occurs body
    | Uprim(_p, args, _) -> List.exists occurs args
    | Uswitch(arg, s, _dbg) ->
        occurs arg ||
        occurs_array_fst s.us_actions_consts ||
        occurs_array_fst s.us_actions_blocks
    | Ustringswitch(arg,sw,d,_) ->
        occurs arg ||
        List.exists (fun (_,e,_) -> occurs e) sw ||
        (match d with None -> false | Some (d, _) -> occurs d)
    | Ustaticfail (_, args) -> List.exists occurs args
    | Ucatch(_, _, body, hdlr, _) -> occurs body || occurs hdlr
    | Utrywith(body, _exn, hdlr, _) -> occurs body || occurs hdlr
    | Uifthenelse(cond, _, ifso, _, ifnot, _) ->
        occurs cond || occurs ifso || occurs ifnot
    | Usequence(u1, u2) -> occurs u1 || occurs u2
    | Uwhile(cond, body, _) -> occurs cond || occurs body
    | Ufor(_id, lo, hi, _dir, body, _) -> occurs lo || occurs hi || occurs body
    | Uassign(id, u) -> id = var || occurs u
    | Usend(_, met, obj, args, _) ->
        occurs met || occurs obj || List.exists occurs args
    | Uunreachable -> false
  and occurs_array_fst a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs (fst a.(i)) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity | Pbytes_to_string | Pbytes_of_string -> 0
  | Pgetglobal _ -> 1
  | Psetglobal _ -> 1
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
    | Uphantom_let (_id, _defining_expr, body) ->
        lambda_size body
    | Uletrec _ ->
        raise Exit (* usually too large *)
    | Uprim(prim, args, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Uswitch(lam, cases, _dbg) ->
        if Array.length cases.us_actions_consts > 1 then size := !size + 5 ;
        if Array.length cases.us_actions_blocks > 1 then size := !size + 5 ;
        lambda_size lam;
        lambda_array_size cases.us_actions_consts ;
        lambda_array_size cases.us_actions_blocks
    | Ustringswitch (lam,sw,d,_) ->
        lambda_size lam ;
       (* as ifthenelse *)
        List.iter
          (fun (_,lam,_) ->
            size := !size+2 ;
            lambda_size lam)
          sw ;
        Misc.may (fun (lam, _loc) -> lambda_size lam) d
    | Ustaticfail (_,args) -> lambda_list_size args
    | Ucatch(_, _, body, handler, _) ->
        incr size; lambda_size body; lambda_size handler
    | Utrywith(body, _id, handler, _) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Uifthenelse(cond, _, ifso, _, ifnot, _) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Usequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Uwhile(cond, body, _) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ufor(_id, low, high, _dir, body, _) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Uassign(_id, lam) ->
        incr size;  lambda_size lam
    | Usend(_, met, obj, args, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
    | Uunreachable -> ()
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter (fun (lam, _) -> lambda_size lam) a in
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
   that is without side-effects *and* not containing function definitions *)

let rec is_pure_clambda = function
    Uvar _ -> true
  | Uconst _ -> true
  | Uprim(p, args, _) -> is_pure_prim p && List.for_all is_pure_clambda args
  | _ -> false

(* Simplify primitive operations on known arguments *)

let make_const c = (Uconst c, Value_const c)
let make_const_ref c =
  make_const(Uconst_ref(Compilenv.new_structured_constant ~shared:true c,
    Some c))
let make_const_int n = make_const (Uconst_int n)
let make_const_ptr n = make_const (Uconst_ptr n)
let make_const_bool b = make_const_ptr(if b then 1 else 0)

let make_integer_comparison cmp x y =
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

let simplif_arith_prim_pure fpc p (args, approxs) dbg =
  let default = (Uprim(p, args, dbg), Value_unknown) in
  match approxs with
  (* int (or enumerated type) *)
  | [ Value_const(Uconst_int n1 | Uconst_ptr n1) ] ->
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
  | [ Value_const(Uconst_int n1 | Uconst_ptr n1);
      Value_const(Uconst_int n2 | Uconst_ptr n2) ] ->
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
      | Plslint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_int (n1 lsl n2)
      | Plsrint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_int (n1 lsr n2)
      | Pasrint when 0 <= n2 && n2 < 8 * Arch.size_int ->
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
      | Plslbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_natint (Nativeint.shift_left n1 n2)
      | Plsrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
          make_const_natint (Nativeint.shift_right_logical n1 n2)
      | Pasrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
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

let simplif_prim_pure fpc p (args, approxs) dbg =
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
  | Pfield n, _, [ Value_const(Uconst_ref(_, Some (Uconst_block(_, l)))) ]
    when n < List.length l ->
      make_const (List.nth l n)
  | Pfield n, [ Uprim(Pmakeblock _, ul, _) ], [approx]
    when n < List.length ul ->
      (List.nth ul n, field_approx n approx)
  (* Strings *)
  | (Pstringlength | Pbyteslength),
     _,
     [ Value_const(Uconst_ref(_, Some (Uconst_string s))) ] ->
      make_const_int (String.length s)
  (* Identity *)
  | (Pidentity | Pbytes_to_string | Pbytes_of_string), [arg1], [app1] ->
      (arg1, app1)
  (* Kind test *)
  | Pisint, _, [a1] ->
      begin match a1 with
      | Value_const(Uconst_int _ | Uconst_ptr _) -> make_const_bool true
      | Value_const(Uconst_ref _) -> make_const_bool false
      | Value_closure _ | Value_tuple _ -> make_const_bool false
      | _ -> (Uprim(p, args, dbg), Value_unknown)
      end
  (* Compile-time constants *)
  | Pctconst c, _, _ ->
      begin match c with
        | Big_endian -> make_const_bool Arch.big_endian
        | Word_size -> make_const_int (8*Arch.size_int)
        | Int_size -> make_const_int (8*Arch.size_int - 1)
        | Max_wosize -> make_const_int ((1 lsl ((8*Arch.size_int) - 10)) - 1 )
        | Ostype_unix -> make_const_bool (Sys.os_type = "Unix")
        | Ostype_win32 -> make_const_bool (Sys.os_type = "Win32")
        | Ostype_cygwin -> make_const_bool (Sys.os_type = "Cygwin")
        | Backend_type ->
            make_const_ptr 0 (* tag 0 is the same as Native here *)
      end
  (* Catch-all *)
  | _ ->
      simplif_arith_prim_pure fpc p (args, approxs) dbg

let simplif_prim fpc p (args, approxs as args_approxs) dbg =
  if List.for_all is_pure_clambda args
  then simplif_prim_pure fpc p args_approxs dbg
  else
    (* XXX : always return the same approxs as simplif_prim_pure? *)
    let approx =
      match p with
      | Pmakeblock(_, Immutable, _kind) ->
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

(* CR mshinwell: "at_call_site" / "function_being_inlined" names may be
   confusing.  Combine into one parameter? *)
let subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst dbg =
  match function_being_inlined with
  | None -> block_subst, dbg
  | Some function_being_inlined ->
    if !Clflags.debug then
  begin
  Format.eprintf "Blocks at call site: %a\n%!" Debuginfo.Current_block.print at_call_site;
  Format.eprintf "Call site: %a\n%!" Debuginfo.Call_site.print function_being_inlined;
  Format.eprintf "Orig dbg: %a\n%!" Debuginfo.print dbg;
  let block_subst, dbg =
      Debuginfo.Block_subst.find_or_add block_subst dbg ~at_call_site
        ~function_being_inlined
  in
  Format.eprintf "New dbg: %a\n\n%!" Debuginfo.print dbg;
  block_subst, dbg
  end
    else
      block_subst, dbg

let rec substitute ~at_call_site ~function_being_inlined ~block_subst
      fpc sb rn ulam =
  match ulam with
    Uvar v ->
      let ulam =
        begin try V.Map.find v sb with Not_found -> ulam end
      in
      block_subst, ulam
  | Uconst _ -> block_subst, ulam
  | Udirect_apply(lbl, args, dbg) ->
      let block_subst, dbg =
        subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst dbg
      in
      let block_subst, args =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn args
      in
      let term = Udirect_apply (lbl, args, dbg) in
      block_subst, term
  | Ugeneric_apply(fn, args, dbg) ->
      let block_subst, dbg =
        subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst dbg
      in
      let block_subst, callee =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn fn
      in
      let block_subst, args =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn args
      in
      let term = Ugeneric_apply (callee, args, dbg) in
      block_subst, term
  | Uclosure(defs, env) ->
      (* Question: should we rename function labels as well?  Otherwise,
         there is a risk that function labels are not globally unique.
         This should not happen in the current system because:
         - Inlined function bodies contain no Uclosure nodes
           (cf. function [lambda_smaller])
         - When we substitute offsets for idents bound by let rec
           in [close], case [Lletrec], we discard the original
           let rec body and use only the substituted term. *)
      let block_subst, env =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn env
      in
      let term = Uclosure (defs, env) in
      block_subst, term
  | Uoffset(u, ofs) ->
      let block_subst, block =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u
      in
      let term = Uoffset (block, ofs) in
      block_subst, term
  | Ulet(str, kind, id, u1, u2) ->
      let block_subst, provenance =
        match VP.provenance id with
        | None -> block_subst, None
        | Some provenance ->
          let block_subst, dbg =
            subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst
              (V.Provenance.debuginfo provenance)
          in
          let provenance =
            V.Provenance.replace_debuginfo provenance dbg
          in
          block_subst, Some provenance
      in
      let id' = VP.rename ?provenance id in
      let block_subst, defining_expr =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, body =
        substitute ~at_call_site ~function_being_inlined ~block_subst fpc
          (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u2
      in
      let term = Ulet (str, kind, id', defining_expr, body) in
      block_subst, term
  | Uphantom_let (id, defining_expr, body) ->
      let block_subst, provenance =
        match VP.provenance id with
        | None -> block_subst, None
        | Some provenance ->
          let block_subst, dbg =
            subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst
              (V.Provenance.debuginfo provenance)
          in
          let provenance =
            V.Provenance.replace_debuginfo provenance dbg
          in
          block_subst, Some provenance
      in
      let id' = VP.rename ?provenance id in
      let defining_expr =
        match defining_expr with
        | None -> None
        | Some defining_expr ->
            match defining_expr with
            | Uphantom_var var ->
                begin match V.Map.find var sb with
                | exception Not_found -> Some defining_expr
                | Uvar var -> Some (Uphantom_var var)
                | Uoffset (Uvar var, offset_in_words) ->
                    Some (Uphantom_offset_var { var; offset_in_words; })
                | _ -> None
                end
            | Uphantom_read_field { var; field; } ->
                begin match V.Map.find var sb with
                | exception Not_found -> Some defining_expr
                | Uvar var -> Some (Uphantom_read_field { var; field; })
                | Uoffset (Uvar var, offset_in_words) ->
                    let field = field + offset_in_words in
                    Some (Uphantom_read_field { var; field; })
                | _ -> None
                end
            | Uphantom_offset_var { var; offset_in_words; } ->
                begin match V.Map.find var sb with
                | exception Not_found -> Some defining_expr
                | Uvar var ->
                    Some (Uphantom_offset_var { var; offset_in_words; })
                | Uoffset (Uvar var, offset_in_words') ->
                    let offset_in_words =
                      offset_in_words + offset_in_words'
                    in
                    Some (Uphantom_offset_var { var; offset_in_words; })
                | _ -> None
                end
            | Uphantom_const _
            | Uphantom_read_symbol_field _ -> Some defining_expr
            | Uphantom_block _ ->
                Misc.fatal_error "[Closure] cannot handle [Uphantom_block]"
      in
      let block_subst, body =
        substitute ~at_call_site ~function_being_inlined ~block_subst fpc
          (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn body
      in
      let term = Uphantom_let (id', defining_expr, body) in
      block_subst, term
  | Uletrec(bindings, body) ->
      let bindings1 =
        List.map (fun (id, rhs) ->
          (VP.var id, VP.rename id, rhs)) bindings
      in
      let sb' =
        List.fold_right (fun (id, id', _) s ->
            V.Map.add id (Uvar (VP.var id')) s)
          bindings1 sb
      in
      let block_subst, bindings_rev =
        List.fold_left (fun (block_subst, bindings_rev) (_id, id', rhs) ->
            let block_subst, defining_expr =
              substitute ~at_call_site ~function_being_inlined ~block_subst
                fpc sb' rn rhs
            in
            block_subst, (id', defining_expr) :: bindings_rev)
          (block_subst, [])
          bindings1
      in
      let block_subst, body =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb' rn body
      in
      let term = Uletrec (List.rev bindings_rev, body) in
      block_subst, term
  | Uprim(p, args, dbg) ->
      let block_subst, sargs =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn args
      in
      let block_subst, dbg =
        subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst dbg
      in
      let (res, _) =
        simplif_prim fpc p (sargs, List.map approx_ulam sargs) dbg in
      block_subst, res
  | Uswitch(arg, sw, dbg) ->
      let block_subst, sarg =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn arg
      in
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
        | Uconst (Uconst_ptr tag) ->
            find_action sw.us_index_consts sw.us_actions_consts tag
        | _ -> None
      in
      begin match action with
      | Some (u, _) ->
          substitute ~at_call_site ~function_being_inlined ~block_subst
            fpc sb rn u
      | None ->
          let block_subst, us_actions_consts =
            substitute_switch_array ~at_call_site ~function_being_inlined
              ~block_subst fpc sb rn sw.us_actions_consts
          in
          let block_subst, us_actions_blocks =
            substitute_switch_array ~at_call_site ~function_being_inlined
              ~block_subst fpc sb rn sw.us_actions_blocks
          in
          let term =
            Uswitch (sarg,
              { sw with
                us_actions_consts;
                us_actions_blocks;
              },
              dbg)
          in
          block_subst, term
      end
  | Ustringswitch (arg, cases, default, dbg) ->
      let block_subst, arg =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn arg
      in
      let block_subst, cases_rev =
        List.fold_left (fun (block_subst, cases_rev) (str, action, loc) ->
            let block_subst, action =
              substitute ~at_call_site ~function_being_inlined ~block_subst
                fpc sb rn action
            in
            block_subst, (str, action, loc) :: cases_rev)
          (block_subst, [])
          cases
      in
      let cases = List.rev cases_rev in
      let block_subst, default =
        match default with
        | None -> block_subst, None
        | Some (default, loc) ->
          let block_subst, default =
            substitute ~at_call_site ~function_being_inlined ~block_subst
              fpc sb rn default
          in
          block_subst, Some (default, loc)
      in
      let term = Ustringswitch (arg, cases, default, dbg) in
      block_subst, term
  | Ustaticfail (nfail, args) ->
      let nfail =
        match rn with
        | Some rn ->
          begin try
            Int.Map.find nfail rn
          with Not_found ->
            fatal_errorf "Closure.split_list: invalid nfail (%d)" nfail
          end
        | None -> nfail
      in
      let block_subst, args =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn args
      in
      let term = Ustaticfail (nfail, args) in
      block_subst, term
  | Ucatch(nfail, ids, u1, u2, loc) ->
      let nfail, rn =
        match rn with
        | Some rn ->
          let new_nfail = next_raise_count () in
          new_nfail, Some (Int.Map.add nfail new_nfail rn)
        | None -> nfail, rn in
      let ids' = List.map VP.rename ids in
      let sb' =
        List.fold_right2
          (fun id id' s -> V.Map.add (VP.var id) (Uvar (VP.var id')) s)
          ids ids' sb
      in
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb' rn u2
      in
      let term = Ucatch (nfail, ids', u1, u2, loc) in
      block_subst, term
  | Utrywith(u1, id, u2, loc) ->
      let id' = VP.rename id in
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u2
      in
      let term = Utrywith (u1, id', u2, loc) in
      block_subst, term
  | Uifthenelse(u1, ifso_loc, u2, ifnot_loc, u3, loc) ->
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      begin match u1 with
        Uconst (Uconst_ptr n) ->
          if n <> 0 then
            substitute ~at_call_site ~function_being_inlined ~block_subst
              fpc sb rn u2
          else
            substitute ~at_call_site ~function_being_inlined ~block_subst
              fpc sb rn u3
      | Uprim(Pmakeblock _, _, _) ->
          substitute ~at_call_site ~function_being_inlined ~block_subst
            fpc sb rn u2
      | su1 ->
          let block_subst, u2 =
            substitute ~at_call_site ~function_being_inlined ~block_subst
              fpc sb rn u2
          in
          let block_subst, u3 =
            substitute ~at_call_site ~function_being_inlined ~block_subst
              fpc sb rn u3
          in
          let term = Uifthenelse (su1, ifso_loc, u2, ifnot_loc, u3, loc) in
          block_subst, term
      end
  | Usequence(u1, u2) ->
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u2
      in
      let term = Usequence (u1, u2) in
      block_subst, term
  | Uwhile(u1, u2, loc) ->
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u2
      in
      let term = Uwhile (u1, u2, loc) in
      block_subst, term
  | Ufor(id, u1, u2, dir, u3, loc) ->
      let id' = VP.rename id in
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u2
      in
      let block_subst, u3 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc (V.Map.add (VP.var id) (Uvar (VP.var id')) sb) rn u3
      in
      let term = Ufor (id', u1, u2, dir, u3, loc) in
      block_subst, term
  | Uassign(id, u) ->
      let id' =
        try
          match V.Map.find id sb with Uvar i -> i | _ -> assert false
        with Not_found ->
          id in
      let block_subst, u =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u
      in
      let term = Uassign (id', u) in
      block_subst, term
  | Usend(k, u1, u2, ul, dbg) ->
      let block_subst, dbg =
        subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst dbg
      in
      let block_subst, u1 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u1
      in
      let block_subst, u2 =
        substitute ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn u2
      in
      let block_subst, ul =
        substitute_list ~at_call_site ~function_being_inlined ~block_subst
          fpc sb rn ul
      in
      let term = Usend (k, u1, u2, ul, dbg) in
      block_subst, term
  | Uunreachable ->
      block_subst, Uunreachable

and substitute_list ~at_call_site ~function_being_inlined ~block_subst
      fpc sb rn terms =
  let block_subst, terms_rev =
    List.fold_left (fun (block_subst, terms_rev) term ->
        let block_subst, term =
          substitute ~at_call_site ~function_being_inlined ~block_subst
            fpc sb rn term
        in
        block_subst, term::terms_rev)
      (block_subst, [])
      terms
  in
  block_subst, List.rev terms_rev

and substitute_switch_array ~at_call_site ~function_being_inlined ~block_subst
      fpc sb rn terms =
  let terms = Array.copy terms in
  let block_subst = ref block_subst in
  for index = 0 to Array.length terms -1 do
    let action, loc = terms.(index) in
    let new_block_subst, action =
      substitute ~at_call_site ~function_being_inlined
        ~block_subst:!block_subst fpc sb rn action
    in
    block_subst := new_block_subst;
    terms.(index) <- action, loc
  done;
  !block_subst, terms

(* Perform an inline expansion *)

let is_simple_argument = function
  | Uvar _  | Uconst _ -> true
  | _ -> false

let no_effects = function
  | Uclosure _ -> true
  | u -> is_pure_clambda u

let rec bind_params_rec ~param_index ~block_subst ~at_call_site
      ~function_being_inlined fpc subst params args ~idents_for_types
      body =
  match (params, args, idents_for_types) with
    ([], [], []) ->
    let _block_subst, term =
      substitute ~block_subst ~at_call_site ~function_being_inlined
        fpc subst (Some Int.Map.empty) body
    in
    term
  | (p1 :: pl, a1 :: al, ident_for_type :: idents_for_types) ->
      let block_subst, provenance =
        match VP.provenance p1 with
        | None -> block_subst, None
        | Some provenance ->
          let block_subst, dbg =
            subst_debuginfo ~at_call_site ~function_being_inlined ~block_subst
              (V.Provenance.debuginfo provenance)
          in
          block_subst, Some (V.Provenance.replace_debuginfo provenance dbg)
      in
      let provenance =
        match provenance with
        | None -> None
        | Some provenance ->
            let provenance =
              V.Provenance.replace_is_parameter provenance
                (Is_parameter.parameter ~index:param_index)
            in
            let provenance =
              (* This ensures that even in the case where the argument of the
                 application has been substituted (e.g. by [Simplif] in
                 the case of an [Alias] let), we still have the ability to
                 recover the type of this argument of the inlined function
                 in the debugger. *)
              match ident_for_type with
              | None -> provenance
              | Some ident_for_type ->
                  V.Provenance.replace_ident_for_type provenance
                    ident_for_type
            in
            Some provenance
      in
      let p1' = VP.rename ?provenance p1 in
      if is_simple_argument a1 then
        let term =
          bind_params_rec ~param_index:(param_index + 1)
            ~block_subst ~at_call_site ~function_being_inlined fpc
            (V.Map.add (VP.var p1) a1 subst) pl al ~idents_for_types body
        in
        let phantom_defining_expr =
          match a1 with
          | Uvar var -> Some (Uphantom_var var)
          | Uconst const -> Some (Uphantom_const const)
          | _ -> None
        in
        Uphantom_let (p1', phantom_defining_expr, term)
      else begin
        let u1, u2 =
          match VP.name p1, a1 with
          | "*opt*", Uprim(Pmakeblock(0, Immutable, kind), [a], dbg) ->
              a, Uprim(Pmakeblock(0, Immutable, kind), [Uvar (VP.var p1')], dbg)
          | _ ->
              a1, Uvar (VP.var p1')
        in
        let body' =
          bind_params_rec ~param_index:(param_index + 1)
            ~block_subst ~at_call_site ~function_being_inlined fpc
            (V.Map.add (VP.var p1) u2 subst) pl al ~idents_for_types body
        in
        if occurs_var (VP.var p1) body then
          Ulet(Immutable, Pgenval, p1', u1, body')
        else
          let lam =
            if no_effects a1 then body'
            else Usequence(a1, body')
          in
          match !Clflags.debug_full with
          | None -> lam
          | Some _ ->
              let defining_expr =
                match u1 with
                | Uvar var -> Some (Uphantom_var var)
                | Uprim (Pfield field, [Uvar var], _dbg) ->
                    Some (Uphantom_read_field { var; field; })
                | _ -> None
              in
              Uphantom_let (p1', defining_expr, lam)
      end
  | (_, _, _) -> assert false

let bind_params ~at_call_site ~function_being_inlined fpc params args
      ~idents_for_types body =
  let block_subst = Debuginfo.Block_subst.empty in
  (* Reverse parameters and arguments to preserve right-to-left
     evaluation order (PR#2910). *)
  bind_params_rec ~param_index:0 ~block_subst ~at_call_site
    ~function_being_inlined fpc V.Map.empty
    (List.rev params) (List.rev args)
    ~idents_for_types:(List.rev idents_for_types) body

(* Check if a lambda term is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar _ -> true
  | Lconst _ -> true
  | Lprim(p, args,_) -> is_pure_prim p && List.for_all is_pure args
  | Levent(lam, _ev) -> is_pure lam
  | _ -> false

let warning_if_forced_inline ~loc ~attribute warning =
  if attribute = Always_inline then
    Location.prerr_warning loc
      (Warnings.Inlining_impossible warning)

(* Generate a direct application *)

let direct_apply fundesc funct ufunct uargs ~loc ~scope ~attribute
      ~idents_for_types =
  if not (List.compare_lengths uargs idents_for_types = 0) then begin
    Misc.fatal_errorf "uargs length %d, idents_for_types length %d, \
        function %s"
      (List.length uargs) (List.length idents_for_types) fundesc.fun_label
  end;
  let app_args =
    if fundesc.fun_closed then uargs else uargs @ [ufunct] in
  let idents_for_types =
    if fundesc.fun_closed then idents_for_types else idents_for_types @ [None]
  in
  let dbg = Debuginfo.of_location loc ~scope in
  let app =
    match fundesc.fun_inline, attribute with
    | _, Never_inline | None, _ ->
        warning_if_forced_inline ~loc ~attribute
          "Function information unavailable";
        Udirect_apply(fundesc.fun_label, app_args, dbg)
    | Some(params, body), _  ->
        let call_site =
          Debuginfo.Call_site.create_from_location fundesc.fun_dbg loc
        in
        bind_params ~at_call_site:scope
          ~function_being_inlined:(Some call_site)
          fundesc.fun_float_const_prop
          params app_args ~idents_for_types body
  in
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app
  else Usequence(ufunct, app)

(* Add [Value_integer] or [Value_constptr] info to the approximation
   of an application *)

let strengthen_approx appl approx =
  match approx_ulam appl with
    (Value_const _) as intapprox ->
      intapprox
  | _ -> approx

(* If a term has approximation Value_integer or Value_constptr and is pure,
   replace it by an integer constant *)

let check_constant_result ~scope lam ulam approx =
  let dbg = Debuginfo.of_location Location.none ~scope in
  match approx with
    Value_const c when is_pure lam -> make_const c
  | Value_global_field (id, i) when is_pure lam ->
      begin match ulam with
      | Uprim(Pfield _, [Uprim(Pgetglobal _, _, _)], _) -> (ulam, approx)
      | _ ->
          let glb =
            Uprim(Pgetglobal (V.create_persistent id), [], dbg)
          in
          Uprim(Pfield i, [glb], dbg), approx
      end
  | _ -> (ulam, approx)

(* Evaluate an expression with known value for its side effects only,
   or discard it if it's pure *)

let sequence_constant_expr lam ulam1 (ulam2, approx2 as res2) =
  if is_pure lam then res2 else (Usequence(ulam1, ulam2), approx2)

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

let close_approx_var fenv cenv id =
  let approx = try V.Map.find id fenv with Not_found -> Value_unknown in
  match approx with
    Value_const c -> make_const c
  | approx ->
      let subst = try V.Map.find id cenv with Not_found -> Uvar id in
      (subst, approx)

let close_var fenv cenv id =
  let (ulam, _app) = close_approx_var fenv cenv id in ulam

let rec close ~scope fenv cenv = function
    Lvar id ->
      close_approx_var fenv cenv id
  | Lconst cst ->
      let str ?(shared = true) cst =
        let name =
          Compilenv.new_structured_constant cst ~shared
        in
        Uconst_ref (name, Some cst)
      in
      let rec transl = function
        | Const_base(Const_int n) -> Uconst_int n
        | Const_base(Const_char c) -> Uconst_int (Char.code c)
        | Const_pointer n -> Uconst_ptr n
        | Const_block (tag, fields) ->
            str (Uconst_block (tag, List.map transl fields))
        | Const_float_array sl ->
            (* constant float arrays are really immutable *)
            str (Uconst_float_array (List.map float_of_string sl))
        | Const_immstring s ->
            str (Uconst_string s)
        | Const_base (Const_string (s, _)) ->
              (* Strings (even literal ones) must be assumed to be mutable...
                 except when OCaml has been configured with
                 -safe-string.  Passing -safe-string at compilation
                 time is not enough, since the unit could be linked
                 with another one compiled without -safe-string, and
                 that one could modify our string literal.  *)
            str ~shared:Config.safe_string (Uconst_string s)
        | Const_base(Const_float x) -> str (Uconst_float (float_of_string x))
        | Const_base(Const_int32 x) -> str (Uconst_int32 x)
        | Const_base(Const_int64 x) -> str (Uconst_int64 x)
        | Const_base(Const_nativeint x) -> str (Uconst_nativeint x)
      in
      make_const (transl cst)
  | Lfunction lfunction as funct ->
      let id_name =
        match !Clflags.debug_full with
        | None -> "*fun*"
        | Some _ ->
            let loc = lfunction.loc in
            if Location.is_none loc then "<anon_fun>"
            else Format.asprintf "<anon_fun:%a>" Location.print_for_debug loc
      in
      close_one_function fenv cenv (Ident.create_local id_name) funct

    (* We convert [f a] to [let a' = a in let f' = f in fun b c -> f' a' b c]
       when fun_arity > nargs *)
  | Lapply{ap_func = funct; ap_args = args; ap_loc = loc;
        ap_inlined = attribute; ap_idents_for_types; _ } ->
      let nargs = List.length args in
      begin match
        close ~scope fenv cenv funct,
        close_list ~scope fenv cenv args
      with
        ((ufunct, Value_closure(fundesc, approx_res)),
         [Uprim(Pmakeblock _, uargs, _)])
        when List.length uargs = - fundesc.fun_arity ->
          (* CR mshinwell: Maybe this could be improved? *)
          let idents_for_types =
            List.map (fun _ -> None) uargs
          in
          let app =
            direct_apply ~scope ~loc ~attribute fundesc funct ufunct uargs
              ~idents_for_types
          in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when nargs = fundesc.fun_arity ->
          let app =
            direct_apply ~scope ~loc ~attribute fundesc funct ufunct uargs
              ~idents_for_types:ap_idents_for_types
          in
          (app, strengthen_approx app approx_res)

      | ((ufunct, (Value_closure(fundesc, _) as fapprox)), uargs)
          when nargs < fundesc.fun_arity ->
        let first_args = List.map (fun arg ->
          (V.create_local "*arg*", arg) ) uargs in
        let final_args =
          Array.to_list (Array.init (fundesc.fun_arity - nargs)
                                    (fun _ -> V.create_local "*arg*")) in
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
        let ap_idents_for_types =
          List.map (fun _lam -> None) internal_args
        in
        let funct_var = V.create_local "*funct*" in
        let fenv = V.Map.add funct_var fapprox fenv in
        let (new_fun, approx) = close ~scope fenv cenv
          (Lfunction{
               kind = Curried;
               params = final_args;
               body = Lapply{ap_should_be_tailcall=false;
                             ap_loc=loc;
                             ap_func=(Lvar funct_var);
                             ap_args=internal_args;
                             ap_inlined=Default_inline;
                             ap_specialised=Default_specialise;
                             ap_idents_for_types};
               loc;
               attr = default_function_attribute})
        in
        let new_fun =
          iter first_args
            (Ulet (Immutable, Pgenval, VP.create funct_var, ufunct, new_fun))
        in
        warning_if_forced_inline ~loc ~attribute "Partial application";
        (new_fun, approx)

      | ((ufunct, Value_closure(fundesc, _approx_res)), uargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let args = List.map (fun arg -> V.create_local "*arg*", arg) uargs in
          let (first_args, rem_args) = split_list fundesc.fun_arity args in
          let first_args = List.map (fun (id, _) -> Uvar id) first_args in
          let rem_args = List.map (fun (id, _) -> Uvar id) rem_args in
          let idents_for_types = List.map (fun _arg -> None) first_args in
          let dbg = Debuginfo.of_location loc ~scope in
          warning_if_forced_inline ~loc ~attribute "Over-application";
          let body =
            Ugeneric_apply(direct_apply ~scope ~loc ~attribute
                              fundesc funct ufunct first_args
                              ~idents_for_types,
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
          let dbg = Debuginfo.of_location loc ~scope in
          warning_if_forced_inline ~loc ~attribute "Unknown function";
          (Ugeneric_apply(ufunct, uargs, dbg), Value_unknown)
      end
  | Lsend(kind, met, obj, args, loc) ->
      let (umet, _) = close ~scope fenv cenv met in
      let (uobj, _) = close ~scope fenv cenv obj in
      let dbg = Debuginfo.of_location loc ~scope in
      (Usend(kind, umet, uobj, close_list ~scope fenv cenv args, dbg),
       Value_unknown)
  | Llet(str, kind, id, lam, body) ->
      let (ulam, alam) = close_named ~scope fenv cenv id lam in
      let body_scope = CB.add_scope scope in
      let provenance =
        match !current_module_path with
        | None -> None
        | Some module_path ->
          let provenance =
            V.Provenance.create ~module_path
              ~debuginfo:(Debuginfo.of_location Location.none ~scope:body_scope)
              ~ident_for_type:id
              Is_parameter.local
          in
          Some provenance
      in
      begin match (str, alam) with
        (Variable, _) ->
          let (ubody, abody) = close ~scope:body_scope fenv cenv body in
          (Ulet(Mutable, kind, VP.create ?provenance id, ulam, ubody), abody)
      | (_, Value_const const)
        when str = Alias || is_pure lam ->
          let ubody, abody =
            close ~scope:body_scope (V.Map.add id alam fenv) cenv body
          in
          Uphantom_let (VP.create ?provenance id,
              Some (Uphantom_const const), ubody),
            abody
      | (_, _) ->
          let (ubody, abody) =
            close ~scope:body_scope (V.Map.add id alam fenv) cenv body
          in
          (Ulet(Immutable, kind, VP.create ?provenance id, ulam, ubody), abody)
      end
  | Lphantom_let (id, defining_expr, body) ->
      let body_scope = CB.add_scope scope in
      let provenance =
        match !current_module_path with
        | None -> None
        | Some module_path ->
          let provenance =
            V.Provenance.create ~module_path
              ~debuginfo:(Debuginfo.of_location Location.none ~scope:body_scope)
              ~ident_for_type:id
              Is_parameter.local
          in
          Some provenance
      in
      let var = VP.create ?provenance id in
      let defining_expr =
        match defining_expr with
        | Lphantom_dead -> None
        | Lphantom_var var -> Some (Uphantom_var var)
        | Lphantom_read_field { var; field; } ->
            Some (Uphantom_read_field { var; field; })
      in
      let body, body_approx = close ~scope:body_scope fenv cenv body in
      Uphantom_let (var, defining_expr, body), body_approx
  | Lletrec(defs, body) ->
      if List.for_all
           (function (_id, Lfunction _) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = V.create_local "*clos*" in
        let fenv_body =
          List.fold_right
            (fun (id, _pos, approx) fenv -> V.Map.add id approx fenv)
            infos fenv in
        let (ubody, approx) = close ~scope fenv_body cenv body in
        let sb =
          List.fold_right
            (fun (id, pos, _approx) sb ->
              V.Map.add id (Uoffset(Uvar clos_ident, pos)) sb)
            infos V.Map.empty in
        let _block_subst, ubody =
          substitute ~at_call_site:scope ~function_being_inlined:None
            ~block_subst:Debuginfo.Block_subst.empty
            !Clflags.float_const_prop sb None ubody
        in
        (Ulet(Immutable, Pgenval, VP.create clos_ident, clos, ubody), approx)
      end else begin
        let new_scope = CB.add_scope scope in
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (id, lam) :: rem ->
            let (udefs, fenv_body) = clos_defs rem in
            let (ulam, approx) =
              close_named ~scope:new_scope fenv cenv id lam
            in
            let provenance =
              match !current_module_path with
              | None -> None
              | Some module_path ->
                let provenance =
                  V.Provenance.create ~module_path
                    ~debuginfo:(Debuginfo.of_location Location.none
                       ~scope:new_scope)
                    ~ident_for_type:id
                    Is_parameter.local
                in
                Some provenance
            in
            ((VP.create ?provenance id, ulam) :: udefs,
              V.Map.add id approx fenv_body) in
        let (udefs, fenv_body) = clos_defs defs in
        let (ubody, approx) = close ~scope:new_scope fenv_body cenv body in
        (Uletrec(udefs, ubody), approx)
      end
  | Lprim(Pdirapply,[funct;arg], loc)
  | Lprim(Prevapply,[arg;funct], loc) ->
      close ~scope fenv cenv (Lapply{ap_should_be_tailcall=false;
                              ap_loc=loc;
                              ap_func=funct;
                              ap_args=[arg];
                              ap_inlined=Default_inline;
                              ap_specialised=Default_specialise;
                              ap_idents_for_types =
                                Lambda.make_idents_for_types [arg];
                              })
  | Lprim(Pgetglobal id, [], loc) as lam ->
      let dbg = Debuginfo.of_location loc ~scope in
      check_constant_result ~scope lam
                            (getglobal dbg id)
                            (Compilenv.global_approx id)
  | Lprim(Pfield n, [lam], loc) ->
      let (ulam, approx) = close ~scope fenv cenv lam in
      let dbg = Debuginfo.of_location loc ~scope in
      check_constant_result ~scope lam (Uprim(Pfield n, [ulam], dbg))
                            (field_approx n approx)
  | Lprim(Psetfield(n, is_ptr, init), [Lprim(Pgetglobal id, [], _); lam], loc)->
      let (ulam, approx) = close ~scope fenv cenv lam in
      if approx <> Value_unknown then
        (!global_approx).(n) <- approx;
      let dbg = Debuginfo.of_location loc ~scope in
      (Uprim(Psetfield(n, is_ptr, init), [getglobal dbg id; ulam], dbg),
       Value_unknown)
  | Lprim(Praise k, [arg], loc) ->
      let (ulam, _approx) = close ~scope fenv cenv arg in
      let dbg = Debuginfo.of_location loc ~scope in
      (Uprim(Praise k, [ulam], dbg),
       Value_unknown)
  | Lprim(p, args, loc) ->
      let dbg = Debuginfo.of_location loc ~scope in
      simplif_prim !Clflags.float_const_prop
                   p (close_list_approx ~scope fenv cenv args) dbg
  | Lswitch(arg, sw, dbg) ->
      let fn fail =
        let (uarg, _) = close ~scope fenv cenv arg in
        let const_index, const_actions, fconst =
          close_switch ~scope fenv cenv sw.sw_consts sw.sw_numconsts fail
        and block_index, block_actions, fblock =
          close_switch ~scope fenv cenv sw.sw_blocks sw.sw_numblocks fail in
        let ulam =
          Uswitch
            (uarg,
             {us_index_consts = const_index;
              us_actions_consts = const_actions;
              us_index_blocks = block_index;
              us_actions_blocks = block_actions},
             Debuginfo.of_location dbg ~scope)
        in
        (fconst (fblock ulam),Value_unknown) in
(* NB: failaction might get copied, thus it should be some Lstaticraise *)
      let fail = sw.sw_failaction in
      begin match fail with
      | None|Some (Lstaticraise (_,_), _) -> fn fail
      | Some (lamfail, fail_loc) ->
          if
            (sw.sw_numconsts - List.length sw.sw_consts) +
            (sw.sw_numblocks - List.length sw.sw_blocks) > 1
          then
            let i = next_raise_count () in
            let ubody,_ = fn (Some (Lstaticraise (i,[]), fail_loc))
            and uhandler,_ = close_new_scope ~scope fenv cenv lamfail in
            let fail_dbg = Debuginfo.of_location fail_loc ~scope in
            Ucatch (i,[],ubody,uhandler, fail_dbg),Value_unknown
          else fn fail
      end
  | Lstringswitch(arg,sw,d,loc) ->
      let uarg,_ = close ~scope fenv cenv arg in
      let usw =
        List.map
          (fun (s,act,loc) ->
            let scope = CB.add_scope scope in
            let uact,_ = close ~scope fenv cenv act in
            let dbg = Debuginfo.of_location loc ~scope in
            s,uact,dbg)
          sw in
      let ud =
        Misc.may_map
          (fun (d, loc) ->
            let scope = CB.add_scope scope in
            let ud,_ = close ~scope fenv cenv d in
            let dbg = Debuginfo.of_location loc ~scope in
            ud, dbg) d in
      let dbg = Debuginfo.of_location loc ~scope in
      Ustringswitch (uarg,usw,ud,dbg),Value_unknown
  | Lstaticraise (i, args) ->
      (Ustaticfail (i, close_list ~scope fenv cenv args), Value_unknown)
  | Lstaticcatch(body, (i, vars), handler, loc) ->
      let body_scope = CB.add_scope scope in
      let (ubody, _) = close ~scope:body_scope fenv cenv body in
      let (uhandler, _) = close_new_scope ~scope fenv cenv handler in
      let vars =
        List.map (fun var ->
            let provenance =
              match !current_module_path with
              | None -> None
              | Some module_path ->
                let provenance =
                  V.Provenance.create ~module_path
                    ~debuginfo:(Debuginfo.of_location loc
                      ~scope:body_scope)
                    ~ident_for_type:var
                    Is_parameter.local
                in
                Some provenance
            in
            VP.create ?provenance var)
          vars
      in
      let dbg = Debuginfo.of_location loc ~scope in
      (Ucatch(i, vars, ubody, uhandler, dbg), Value_unknown)
  | Ltrywith(body, id, handler, loc) ->
      let (ubody, _) = close_new_scope ~scope fenv cenv body in
      let handler_scope = CB.add_scope scope in
      let (uhandler, _) = close ~scope:handler_scope fenv cenv handler in
      let provenance =
        match !current_module_path with
        | None -> None
        | Some module_path ->
          let provenance =
            V.Provenance.create ~module_path
              ~debuginfo:(Debuginfo.of_location loc
                ~scope:handler_scope)
              ~ident_for_type:id
              Is_parameter.local
          in
          Some provenance
      in
      let dbg = Debuginfo.of_location loc ~scope in
      (Utrywith(ubody, VP.create ?provenance id, uhandler, dbg), Value_unknown)
  | Lifthenelse(arg, ifso_loc, ifso, ifnot_loc, ifnot, loc) ->
      begin match close ~scope fenv cenv arg with
        (uarg, Value_const (Uconst_ptr n)) ->
          sequence_constant_expr arg uarg
            (close ~scope fenv cenv (if n = 0 then ifnot else ifso))
      | (uarg, _ ) ->
          let dbg = Debuginfo.of_location loc ~scope in
          let ifso_scope = CB.add_scope scope in
          let ifso_dbg = Debuginfo.of_location ifso_loc ~scope:ifso_scope in
          let ifnot_scope = CB.add_scope scope in
          let ifnot_dbg = Debuginfo.of_location ifnot_loc ~scope:ifnot_scope in
          let (uifso, _) = close ~scope:ifso_scope fenv cenv ifso in
          let (uifnot, _) = close ~scope:ifnot_scope fenv cenv ifnot in
          (Uifthenelse(uarg, ifso_dbg, uifso, ifnot_dbg, uifnot, dbg),
            Value_unknown)
      end
  | Lsequence(lam1, lam2) ->
      let (ulam1, _) = close ~scope fenv cenv lam1 in
      let (ulam2, approx) = close ~scope fenv cenv lam2 in
      (Usequence(ulam1, ulam2), approx)
  | Lwhile(cond, body, loc) ->
      let (ucond, _) = close ~scope fenv cenv cond in
      let body_scope = CB.add_scope scope in
      let (ubody, _) = close ~scope:body_scope fenv cenv body in
      let dbg = Debuginfo.of_location loc ~scope:body_scope in
      (Uwhile(ucond, ubody, dbg), Value_unknown)
  | Lfor(id, lo, hi, dir, body, loc) ->
      let (ulo, _) = close ~scope fenv cenv lo in
      let (uhi, _) = close ~scope fenv cenv hi in
      let body_scope = CB.add_scope scope in
      let (ubody, _) = close_new_scope ~scope:body_scope fenv cenv body in
      let dbg = Debuginfo.of_location loc ~scope:body_scope in
      let provenance =
        match !current_module_path with
        | None -> None
        | Some module_path ->
          let provenance =
            V.Provenance.create ~module_path
              ~debuginfo:(Debuginfo.of_location loc ~scope:body_scope)
              ~ident_for_type:id
              Is_parameter.local
          in
          Some provenance
      in
      (Ufor(VP.create ?provenance id, ulo, uhi, dir, ubody, dbg), Value_unknown)
  | Lassign(id, lam) ->
      let (ulam, _) = close ~scope fenv cenv lam in
      (Uassign(id, ulam), Value_unknown)
  | Levent(lam, ev) ->
      let prev_module_path = !current_module_path in
      begin match ev.lev_kind with
      | Lev_module_definition path ->
        current_module_path := Some (Path.Pident path)
      | _ -> ()
      end;
      let term, approx = close ~scope fenv cenv lam in
      current_module_path := prev_module_path;
      term, approx
  | Lifused _ ->
      assert false

and close_list ~scope fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close ~scope fenv cenv lam in
      ulam :: close_list ~scope fenv cenv rem

and close_list_approx ~scope fenv cenv = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close ~scope fenv cenv lam in
      let (ulams, approxs) = close_list_approx ~scope fenv cenv rem in
      (ulam :: ulams, approx :: approxs)

and close_named ~scope fenv cenv id = function
    Lfunction _ as funct ->
      close_one_function fenv cenv id funct
  | lam ->
      close ~scope fenv cenv lam

and close_new_scope ~scope fenv cenv lam =
  let scope = CB.add_scope scope in
  close ~scope fenv cenv lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  let fun_defs =
    List.flatten
      (List.map
         (function
           | (id, Lfunction{kind; params; body; attr; loc}) ->
               Simplif.split_default_wrapper ~id ~kind ~params
                 ~body ~attr ~loc
           | _ -> assert false
         )
         fun_defs)
  in
  let inline_attribute = match fun_defs with
    | [_, Lfunction{attr = { inline; }}] -> inline
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
          (id, Lfunction{kind; params; body; loc}) ->
            let label = Compilenv.make_symbol (Some (V.unique_name id)) in
            let arity = List.length params in
            let fun_dbg =
              Debuginfo.Function.create_from_location loc
                ~human_name:(V.name id)
                ~module_path:!current_module_path
            in
            let fundesc =
              {fun_label = label;
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_dbg;
               fun_closed = initially_closed;
               fun_inline = None;
               fun_float_const_prop = !Clflags.float_const_prop} in
            (id, params, body, fundesc, fun_dbg)
        | (_, _) -> fatal_error "Closure.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun (id, _params, _body, fundesc, _fun_dbg) fenv ->
        V.Map.add id (Value_closure(fundesc, Value_unknown)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (_id, _params, _body, fundesc, _fun_dbg) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref initially_closed in
  (* Translate each function definition *)
  let clos_fundef (id, params, body, fundesc, fun_dbg) env_pos =
    let env_param = V.create_local "*env*" in
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv in
    let cenv_body =
      List.fold_right2
        (fun (id, _params, _body, _fundesc, _fun_dbg) pos env ->
          V.Map.add id (Uoffset(Uvar env_param, pos - env_pos)) env)
        uncurried_defs clos_offsets cenv_fv in
    let scope = CB.toplevel in
    let (ubody, approx) = close ~scope fenv_rec cenv_body body in
    if !useless_env && occurs_var env_param ubody then raise NotClosed;
    let fun_params = if !useless_env then params else params @ [env_param] in
    let params =
      List.map (fun ident ->
          let provenance =
            if ident == env_param then
              None
            else
              match Debuginfo.Function.module_path fun_dbg with
              | None -> None
              | Some module_path ->
                let provenance =
                  V.Provenance.create ~module_path
                    ~debuginfo:(Debuginfo.of_function fun_dbg)
                    ~ident_for_type:ident
                    Is_parameter.local
                in
                Some provenance
          in
          VP.create ?provenance ident)
        fun_params
    in
    let f =
      {
        label  = fundesc.fun_label;
        arity  = fundesc.fun_arity;
        params;
        body   = ubody;
        dbg = fun_dbg;
        env = Some env_param;
      }
    in
    (* give more chance of function with default parameters (i.e.
       their wrapper functions) to be inlined *)
    let n =
      List.fold_left
        (fun n id -> n + if V.name id = "*opt*" then 8 else 1)
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
      | Always_inline -> max_int
      | Never_inline -> min_int
      | Unroll _ -> assert false
    in
    if lambda_smaller ubody threshold
    then fundesc.fun_inline <- Some(params, ubody);

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
          (fun (_id, _params, _body, fundesc, _dbg) ->
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
  (Uclosure(clos, List.map (close_var fenv cenv) fv), infos)

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
  | (clos, (i, _, approx) :: _) when id = i -> (clos, approx)
  | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch ~scope fenv cenv cases num_keys default =
  let ncases = List.length cases in
  let index = Array.make num_keys 0
  and store = Storer.mk_store () in

  (* First default case *)
  begin match default with
  | Some (def, loc) when ncases < num_keys ->
      assert (store.act_store () (loc, def) = 0)
  | _ -> ()
  end ;
  (* Then all other cases *)
  List.iter
    (fun (key,lam,loc) ->
     index.(key) <- store.act_store () (loc, lam))
    cases ;

  (*  Explicit sharing with catch/exit, as switcher compilation may
      later unshare *)
  let acts = store.act_get_shared () in
  let hs = ref (fun e -> e) in

  (* Compile actions *)
  let actions =
    Array.map
      (function
        | Single (loc, lam)
        | Shared (loc, (Lstaticraise (_,[]) as lam)) ->
            let scope = CB.add_scope scope in
            let dbg = Debuginfo.of_location ~scope loc in
            let ulam,_ = close_new_scope ~scope fenv cenv lam in
            ulam, dbg
        | Shared (loc, lam) ->
            let scope = CB.add_scope scope in
            let dbg = Debuginfo.of_location ~scope loc in
            let ulam,_ = close_new_scope ~scope fenv cenv lam in
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
            hs := (fun e -> Ucatch (i,[],ohs e,ulam,dbg)) ;
            Ustaticfail (i,[]), dbg)
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
    | Uconst_int _ | Uconst_ptr _ -> ()
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
    | Uphantom_let (_id, _defining_expr, body) -> ulam body
    | Uletrec (l, u) -> List.iter (fun (_, u) -> ulam u) l; ulam u
    | Uprim (_, ul, _) -> List.iter ulam ul
    | Uswitch (u, sl, _dbg) ->
        ulam u;
        Array.iter (fun (act, _) -> ulam act) sl.us_actions_consts;
        Array.iter (fun (act, _) -> ulam act) sl.us_actions_blocks
    | Ustringswitch (u,sw,d,_) ->
        ulam u ;
        List.iter (fun (_,act,_) -> ulam act) sw ;
        Misc.may (fun (default, _) -> ulam default) d
    | Ustaticfail (_, ul) -> List.iter ulam ul
    | Ucatch (_, _, u1, u2, _)
    | Utrywith (u1, _, u2, _)
    | Usequence (u1, u2)
    | Uwhile (u1, u2, _)  -> ulam u1; ulam u2
    | Uifthenelse (u1, _, u2, _, u3, _)
    | Ufor (_, u1, u2, _, u3, _) -> ulam u1; ulam u2; ulam u3
    | Uassign (_, u) -> ulam u
    | Usend (_, u1, u2, ul, _) -> ulam u1; ulam u2; List.iter ulam ul
    | Uunreachable -> ()
  in
  approx a

let reset () =
  global_approx := [||];
  function_nesting_depth := 0;
  current_module_path := None

(* The entry point *)

let intro size lam =
  reset ();
  current_module_path :=
    Some (Printtyp.rewrite_double_underscore_paths Env.initial_safe_string
      (Path.Pident (Ident.create_persistent (Compilenv.current_unit_name ()))));
  let id = Compilenv.make_symbol None in
  global_approx := Array.init size (fun i -> Value_global_field (id, i));
  Compilenv.set_global_approx(Value_tuple !global_approx);
  let scope = CB.toplevel in
  let (ulam, _approx) = close ~scope V.Map.empty V.Map.empty lam in
  let opaque =
    !Clflags.opaque
    || Env.is_imported_opaque (Compilenv.current_unit_name ())
  in
  if opaque
  then Compilenv.set_global_approx(Value_unknown)
  else collect_exported_structured_constants (Value_tuple !global_approx);
  global_approx := [||];
  ulam
