(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Formatmsg
open Asttypes
open Primitive
open Types
open Lambda


let rec struct_const ppf = function
    Const_base(Const_int n) -> print_int n
  | Const_base(Const_char c) ->
      printf "'%s'" (Char.escaped c)
  | Const_base(Const_string s) ->
      printf "\"%s\"" (String.escaped s)
  | Const_base(Const_float s) ->
      print_string s
  | Const_pointer n -> printf "%ia" n
  | Const_block(tag, []) ->
      printf "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts ppf scl =
        List.iter (fun sc -> printf "@ %a" struct_const sc) scl in
      printf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      print_string "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> print_space(); print_string f) fl in
      printf "@[<1[|@[%s%a@]|]@]" f1 floats fl

let print_id ppf id = Ident.print id

let print_boxed_integer name bi =
  match bi with
    Pnativeint -> printf "Nativeint.%s" name
  | Pint32 -> printf "Int32.%s" name
  | Pint64 -> printf "Int64.%s" name

let primitive ppf = function
    Pidentity -> print_string "id"
  | Pignore -> print_string "ignore"
  | Pgetglobal id -> printf "global %a" print_id id
  | Psetglobal id -> printf "setglobal %a" print_id id
  | Pmakeblock(tag, Immutable) -> printf "makeblock %i" tag
  | Pmakeblock(tag, Mutable) -> printf "makemutable %i" tag
  | Pfield n -> printf "field %i" n
  | Psetfield(n, ptr) ->
      print_string (if ptr then "setfield_ptr " else "setfield_imm ");
      print_int n
  | Pfloatfield n -> printf "floatfield %i" n
  | Psetfloatfield n -> printf "setfloatfield %i" n
  | Pccall p -> print_string p.prim_name
  | Praise -> print_string "raise"
  | Psequand -> print_string "&&"
  | Psequor -> print_string "||"
  | Pnot -> print_string "not"
  | Pnegint -> print_string "~"
  | Paddint -> print_string "+"
  | Psubint -> print_string "-"
  | Pmulint -> print_string "*"
  | Pdivint -> print_string "/"
  | Pmodint -> print_string "mod"
  | Pandint -> print_string "and"
  | Porint -> print_string "or"
  | Pxorint -> print_string "xor"
  | Plslint -> print_string "lsl"
  | Plsrint -> print_string "lsr"
  | Pasrint -> print_string "asr"
  | Pintcomp(Ceq) -> print_string "=="
  | Pintcomp(Cneq) -> print_string "!="
  | Pintcomp(Clt) -> print_string "<"
  | Pintcomp(Cle) -> print_string "<="
  | Pintcomp(Cgt) -> print_string ">"
  | Pintcomp(Cge) -> print_string ">="
  | Poffsetint n -> print_int n; print_string "+"
  | Poffsetref n -> print_int n; print_string "+:="
  | Pintoffloat -> print_string "int_of_float"
  | Pfloatofint -> print_string "float_of_int"
  | Pnegfloat -> print_string "~."
  | Pabsfloat -> print_string "abs."
  | Paddfloat -> print_string "+."
  | Psubfloat -> print_string "-."
  | Pmulfloat -> print_string "*."
  | Pdivfloat -> print_string "/."
  | Pfloatcomp(Ceq) -> print_string "==."
  | Pfloatcomp(Cneq) -> print_string "!=."
  | Pfloatcomp(Clt) -> print_string "<."
  | Pfloatcomp(Cle) -> print_string "<=."
  | Pfloatcomp(Cgt) -> print_string ">."
  | Pfloatcomp(Cge) -> print_string ">=."
  | Pstringlength -> print_string "string.length"
  | Pstringrefu -> print_string "string.unsafe_get"
  | Pstringsetu -> print_string "string.unsafe_set"
  | Pstringrefs -> print_string "string.get"
  | Pstringsets -> print_string "string.set"
  | Parraylength _ -> print_string "array.length"
  | Pmakearray _ -> print_string "makearray "
  | Parrayrefu _ -> print_string "array.unsafe_get"
  | Parraysetu _ -> print_string "array.unsafe_set"
  | Parrayrefs _ -> print_string "array.get"
  | Parraysets _ -> print_string "array.set"
  | Pisint -> print_string "isint"
  | Pbittest -> print_string "testbit"
  | Pbintofint bi -> print_boxed_integer "of_int" bi
  | Pintofbint bi -> print_boxed_integer "to_int" bi
  | Pnegbint bi -> print_boxed_integer "neg" bi
  | Paddbint bi -> print_boxed_integer "add" bi
  | Psubbint bi -> print_boxed_integer "sub" bi
  | Pmulbint bi -> print_boxed_integer "mul" bi
  | Pdivbint bi -> print_boxed_integer "div" bi
  | Pmodbint bi -> print_boxed_integer "mod" bi
  | Pandbint bi -> print_boxed_integer "and" bi
  | Porbint bi -> print_boxed_integer "or" bi
  | Pxorbint bi -> print_boxed_integer "xor" bi
  | Plslbint bi -> print_boxed_integer "lsl" bi
  | Plsrbint bi -> print_boxed_integer "lsr" bi
  | Pasrbint bi -> print_boxed_integer "asr" bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" bi
  | Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" bi

let rec lam ppf = function
    Lvar id ->
      print_id ppf id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply(lfun, largs) ->
      let lams ppf largs =
        List.iter (fun l -> printf "@ %a" lam l) largs in
      printf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Lfunction(kind, params, body) ->
      let pr_params ppf params =
        match kind with
        | Curried ->
            List.iter (fun param -> printf "@ %a" print_id param) params
        | Tupled ->
            print_string " (";
            let first = ref true in
            List.iter
              (fun param ->
                if !first then first := false else printf ",@ ";
                print_id ppf param)
              params;
            print_string ")" in
      printf "@[<2>(function%a@ %a)@]" pr_params params lam body
  | Llet(str, id, arg, body) ->
      let rec letbody = function
        | Llet(str, id, arg, body) ->
            printf "@ @[<2>%a@ %a@]" print_id id lam arg;
            letbody body
        | expr -> expr in
      printf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]" print_id id lam arg;
      let expr = letbody body in
      printf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then print_space() else spc := true;
            printf "@[<2>%a@ %a@]" print_id id lam l)
          id_arg_list in
      printf "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs) ->
      let lams ppf largs =
        List.iter (fun l -> printf "@ %a" lam l) largs in
      printf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then print_space() else spc := true;
           printf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.sw_consts;
       List.iter
         (fun (n, l) ->
           if !spc then print_space() else spc := true;
           printf "@[<hv 1>case tag %i:@ %a@]" n lam l)
         sw.sw_blocks in
      printf
       "@[<1>(%s%a@ @[<v 0>%a@])@]"
       (if sw.sw_checked then "switch-checked " else "switch ")
       lam larg switch sw
  | Lstaticfail ->
      print_string "exit"
  | Lcatch(lbody, lhandler) ->
      printf "@[<2>(catch@ %a@;<1 -1>with@ %a)@]" lam lbody lam lhandler
  | Ltrywith(lbody, param, lhandler) ->
      printf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody print_id param lam lhandler
  | Lifthenelse(lcond, lif, lelse) ->
      printf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      printf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      printf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      printf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       print_id param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      printf "@[<2>(assign@ %a@ %a)@]" print_id id lam expr
  | Lsend (met, obj, largs) ->
      let args ppf largs =
        List.iter (fun l -> printf "@ %a" lam l) largs in
      printf "@[<2>(send@ %a@ %a%a)@]" lam obj lam met args largs
  | Levent(expr, ev) ->
      let kind = 
       match ev.lev_kind with
       | Lev_before -> "before"
       | Lev_after _  -> "after"
       | Lev_function -> "funct-body" in
      printf "@[<2>(%s %i@ %a)@]" kind ev.lev_loc lam expr
  | Lifused(id, expr) ->
      printf "@[<2>(ifused@ %a@ %a)@]" print_id id lam expr

and sequence ppf = function
    Lsequence(l1, l2) ->
      printf "%a@ %a" sequence l1 sequence l2
  | Llet(str, id, arg, body) ->
      printf "@[<2>let@ %a@ %a@]@ %a" print_id id lam arg sequence body
  | l ->
      lam ppf l

let structured_constant cst = printf "%a" struct_const cst

let lambda l = printf "%a" lam l
