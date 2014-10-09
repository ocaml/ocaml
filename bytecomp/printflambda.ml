(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


open Format
open Ext_types
open Abstract_identifiers
open Flambda

let rec lam ppf = function
  | Fsymbol (symbol,_) ->
      Symbol.print ppf symbol
  | Fvar (id,_) ->
      Variable.print ppf id
  | Fconst (cst,_) ->
      const ppf cst
  | Fapply({ap_function; ap_arg; ap_kind},_) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let direct = match ap_kind with Indirect -> "" | Direct _ -> "*" in
      fprintf ppf "@[<2>(apply%s@ %a%a)@]" direct lam ap_function lams ap_arg
  | Ffunction({fu_closure;fu_fun;fu_relative_to = None},_) ->
      fprintf ppf "@[<2>(function@ %a@ %a)@]" Closure_function.print fu_fun lam fu_closure
  | Ffunction({fu_closure;fu_fun;fu_relative_to = Some rel},_) ->
      fprintf ppf "@[<2>(function_relative@ %a - %a@ %a)@]"
        Closure_function.print fu_fun Closure_function.print rel lam fu_closure
  | Fvariable_in_closure({vc_closure;vc_fun;vc_var},_) ->
      fprintf ppf "@[<2>(var@ %a@ %a@ %a)@]"
        Closure_variable.print vc_var Closure_function.print vc_fun lam vc_closure
  | Fclosure({cl_fun;cl_free_var;cl_specialised_arg},_) ->
      let idents ppf =
        List.iter (fprintf ppf "@ %a" Variable.print) in
      let funs ppf =
        VarMap.iter (fun var f ->
            fprintf ppf "@ (fun %a@[<2>%a@]@ @[<2>%a@])"
              Variable.print var idents f.params lam f.body) in
      let lams ppf =
        VarMap.iter (fun id v -> fprintf ppf "@ %a = %a"
                        Variable.print id lam v) in
      let spec ppf spec_args =
        if not (VarMap.is_empty spec_args)
        then begin
          fprintf ppf "@ with";
          VarMap.iter (fun id id' -> fprintf ppf "@ %a <- %a"
                          Variable.print id Variable.print id')
            spec_args
        end
      in
      fprintf ppf "@[<2>(closure%a %a%a)@]" funs cl_fun.funs lams
        cl_free_var spec cl_specialised_arg
  | Flet(str, id, arg, body,_) ->
      let rec letbody ul = match ul with
        | Flet(str, id, arg, body,_) ->
            fprintf ppf "@ @[<2>%a@ %a@]" Variable.print id lam arg;
            letbody body
        | _ -> ul in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" Variable.print id lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Fletrec(id_arg_list, body,_) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<2>%a@ %a@]" Variable.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Fprim(prim, largs, _,_) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim lams largs
  | Fswitch(larg, sw,_) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
          sw.fs_consts;
        List.iter
          (fun (n, l) ->
             if !spc then fprintf ppf "@ " else spc := true;
             fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.fs_blocks ;
        begin match sw.fs_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
        "@[<1>(%s(%i,%i) %a@ @[<v 0>%a@])@]"
        (match sw.fs_failaction with None -> "switch*" | _ -> "switch")
        (IntSet.cardinal sw.fs_numconsts)
        (IntSet.cardinal sw.fs_numblocks)
        lam larg switch sw
  | Fstaticraise (i, ls,_)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %a%a)@]" Static_exception.print i lams ls;
  | Fstaticcatch(i, vars, lbody, lhandler,_) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%a%a)@ %a)@]"
        lam lbody Static_exception.print i
        (fun ppf vars -> match vars with
           | [] -> ()
           | _ ->
               List.iter
                 (fun x -> fprintf ppf " %a" Variable.print x)
                 vars)
        vars
        lam lhandler
  | Ftrywith(lbody, param, lhandler,_) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Variable.print param lam lhandler
  | Fifthenelse(lcond, lif, lelse,_) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Fsequence(l1, l2,_) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Fwhile(lcond, lbody,_) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Ffor(param, lo, hi, dir, body,_) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
        Variable.print param lam lo
        (match dir with Asttypes.Upto -> "to" | Asttypes.Downto -> "downto")
        lam hi lam body
  | Fassign(id, expr,_) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Variable.print id lam expr
  | Fsend (k, met, obj, largs, _,_) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Lambda.Self then "self" else if k = Lambda.Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Fevent(expr, ev, _) ->
      let open Lambda in
      let kind =
        match ev.lev_kind with
        | Lev_before -> "before"
        | Lev_after _  -> "after"
        | Lev_function -> "funct-body" in
      fprintf ppf "@[<2>(%s %s(%i)%s:%i-%i@ %a)@]" kind
              ev.lev_loc.Location.loc_start.Lexing.pos_fname
              ev.lev_loc.Location.loc_start.Lexing.pos_lnum
              (if ev.lev_loc.Location.loc_ghost then "<ghost>" else "")
              ev.lev_loc.Location.loc_start.Lexing.pos_cnum
              ev.lev_loc.Location.loc_end.Lexing.pos_cnum
              lam expr
  | Funreachable _ ->
      fprintf ppf "unreachable"

and sequence ppf ulam = match ulam with
  | Fsequence(l1, l2,_) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | _ -> lam ppf ulam

and const ppf c = let open Asttypes in match c with
  | Fconst_base(Const_int n) -> fprintf ppf "%i" n
  | Fconst_base(Const_char c) -> fprintf ppf "%C" c
  | Fconst_base(Const_string s) -> fprintf ppf "%S" s
  | Fconst_immstring s -> fprintf ppf "#%S" s
  | Fconst_base(Const_float f) -> fprintf ppf "%s" f
  | Fconst_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Fconst_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Fconst_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Fconst_pointer n -> fprintf ppf "%ia" n
  | Fconst_float_array [] ->
      fprintf ppf "[| |]"
  | Fconst_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let function_declarations ppf fd =
  let idents ppf =
    List.iter (fprintf ppf "@ %a" Variable.print) in
  let funs ppf =
    VarMap.iter (fun var f ->
        fprintf ppf "@ (fun@ %a@[<2>%a@]@ @[<2>%a@])"
          Variable.print var idents f.params lam f.body) in
  fprintf ppf "@[<2>(%a)@]" funs fd.funs


let flambda ppf ulam =
  fprintf ppf "%a@." lam ulam
