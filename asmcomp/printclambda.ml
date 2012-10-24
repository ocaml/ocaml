(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


open Format
open Asttypes
open Clambda

let rec pr_idents ppf = function
  | [] -> ()
  | h::t -> fprintf ppf "%a %a" Ident.print h pr_idents t

let rec lam ppf = function
  | Uvar id ->
      Ident.print ppf id
  | Uconst (cst,_) ->
      Printlambda.structured_constant ppf cst
  | Udirect_apply(f, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply*@ %s %a)@]" f lams largs
  | Ugeneric_apply(lfun, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Uclosure(clos, fv) ->
      let idents ppf =
        List.iter (fprintf ppf "@ %a" Ident.print)in
      let one_fun ppf f =
        fprintf ppf "(fun@ %s@ %d @[<2>%a@] @[<2>%a@])"
          f.label f.arity idents f.params lam f.body in
      let funs ppf =
        List.iter (fprintf ppf "@ %a" one_fun) in
      let lams ppf =
        List.iter (fprintf ppf "@ %a" lam) in
      fprintf ppf "@[<2>(closure@ %a %a)@]" funs clos lams fv
  | Uoffset(l,i) -> fprintf ppf "@[<2>(offset %a %d)@]" lam l i
  | Ulet(id, arg, body) ->
      let rec letbody ul = match ul with
        | Ulet(id, arg, body) ->
            fprintf ppf "@ @[<2>%a@ %a@]" Ident.print id lam arg;
            letbody body
        | _ -> ul in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" Ident.print id lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Uletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Uprim(prim, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" Printlambda.primitive prim lams largs
  | Uswitch(larg, sw) ->
      let switch ppf sw =
        let spc = ref false in
        for i = 0 to Array.length sw.us_index_consts - 1 do
          let n = sw.us_index_consts.(i)
          and l = sw.us_actions_consts.(i) in
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l;
        done;
        for i = 0 to Array.length sw.us_index_blocks - 1 do
          let n = sw.us_index_blocks.(i)
          and l = sw.us_actions_blocks.(i) in
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l;
        done in
      fprintf ppf
       "@[<1>(switch %a@ @[<v 0>%a@])@]"
        lam larg switch sw
  | Ustaticfail (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Ucatch(i, vars, lbody, lhandler) ->
      fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]"
        lam lbody i
        (fun ppf vars -> match vars with
          | [] -> ()
          | _ ->
              List.iter
                (fun x -> fprintf ppf " %a" Ident.print x)
                vars)
        vars
        lam lhandler
  | Utrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Uifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Usequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Uwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Ufor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Uassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Usend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Lambda.Self then "self" else if k = Lambda.Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs

and sequence ppf ulam = match ulam with
  | Usequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | _ -> lam ppf ulam

let clambda ppf ulam =
  fprintf ppf "%a@." lam ulam
