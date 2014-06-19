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
open Primitive
open Types
open Lambda


let rec struct_const ppf = function
  | Const_base(Const_int n) -> fprintf ppf "%i" n
  | Const_base(Const_char c) -> fprintf ppf "%C" c
  | Const_base(Const_string (s, _)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base(Const_float f) -> fprintf ppf "%s" f
  | Const_base(Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base(Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base(Const_nativeint n) -> fprintf ppf "%nin" n
  | Const_pointer n -> fprintf ppf "%ia" n
  | Const_block(tag, []) ->
      fprintf ppf "[%i]" tag
  | Const_block(tag, sc1::scl) ->
      let sconsts ppf scl =
        List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl in
      fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_float_array [] ->
      fprintf ppf "[| |]"
  | Const_float_array (f1 :: fl) ->
      let floats ppf fl =
        List.iter (fun f -> fprintf ppf "@ %s" f) fl in
      fprintf ppf "@[<1>[|@[%s%a@]|]@]" f1 floats fl

let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let print_boxed_integer name ppf bi =
  fprintf ppf "%s_%s" (boxed_integer_name bi) name

let print_boxed_integer_conversion ppf bi1 bi2 =
  fprintf ppf "%s_of_%s" (boxed_integer_name bi2) (boxed_integer_name bi1)

let boxed_integer_mark name = function
  | Pnativeint -> Printf.sprintf "Nativeint.%s" name
  | Pint32 -> Printf.sprintf "Int32.%s" name
  | Pint64 -> Printf.sprintf "Int64.%s" name

let print_boxed_integer name ppf bi =
  fprintf ppf "%s" (boxed_integer_mark name bi);;

let print_bigarray name unsafe kind ppf layout =
  fprintf ppf "Bigarray.%s[%s,%s]"
    (if unsafe then "unsafe_"^ name else name)
    (match kind with
     | Pbigarray_unknown -> "generic"
     | Pbigarray_float32 -> "float32"
     | Pbigarray_float64 -> "float64"
     | Pbigarray_sint8 -> "sint8"
     | Pbigarray_uint8 -> "uint8"
     | Pbigarray_sint16 -> "sint16"
     | Pbigarray_uint16 -> "uint16"
     | Pbigarray_int32 -> "int32"
     | Pbigarray_int64 -> "int64"
     | Pbigarray_caml_int -> "camlint"
     | Pbigarray_native_int -> "nativeint"
     | Pbigarray_complex32 -> "complex32"
     | Pbigarray_complex64 -> "complex64")
    (match layout with
    |  Pbigarray_unknown_layout -> "unknown"
     | Pbigarray_c_layout -> "C"
     | Pbigarray_fortran_layout -> "Fortran")

let record_rep ppf r =
  match r with
  | Record_regular -> fprintf ppf "regular"
  | Record_float -> fprintf ppf "float"
;;

let string_of_loc_kind = function
  | Loc_FILE -> "loc_FILE"
  | Loc_LINE -> "loc_LINE"
  | Loc_MODULE -> "loc_MODULE"
  | Loc_POS -> "loc_POS"
  | Loc_LOC -> "loc_LOC"

let primitive ppf = function
  | Pidentity -> fprintf ppf "id"
  | Pignore -> fprintf ppf "ignore"
  | Prevapply _ -> fprintf ppf "revapply"
  | Pdirapply _ -> fprintf ppf "dirapply"
  | Ploc kind -> fprintf ppf "%s" (string_of_loc_kind kind)
  | Pgetglobal id -> fprintf ppf "global %a" Ident.print id
  | Psetglobal id -> fprintf ppf "setglobal %a" Ident.print id
  | Pmakeblock(tag, Immutable) -> fprintf ppf "makeblock %i" tag
  | Pmakeblock(tag, Mutable) -> fprintf ppf "makemutable %i" tag
  | Pfield n -> fprintf ppf "field %i" n
  | Psetfield(n, ptr) ->
      let instr = if ptr then "setfield_ptr " else "setfield_imm " in
      fprintf ppf "%s%i" instr n
  | Pfloatfield n -> fprintf ppf "floatfield %i" n
  | Psetfloatfield n -> fprintf ppf "setfloatfield %i" n
  | Pduprecord (rep, size) -> fprintf ppf "duprecord %a %i" record_rep rep size
  | Plazyforce -> fprintf ppf "force"
  | Pccall p -> fprintf ppf "%s" p.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint -> fprintf ppf "/"
  | Pmodint -> fprintf ppf "mod"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp(Ceq) -> fprintf ppf "=="
  | Pintcomp(Cneq) -> fprintf ppf "!="
  | Pintcomp(Clt) -> fprintf ppf "<"
  | Pintcomp(Cle) -> fprintf ppf "<="
  | Pintcomp(Cgt) -> fprintf ppf ">"
  | Pintcomp(Cge) -> fprintf ppf ">="
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i"n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pfloatcomp(Ceq) -> fprintf ppf "==."
  | Pfloatcomp(Cneq) -> fprintf ppf "!=."
  | Pfloatcomp(Clt) -> fprintf ppf "<."
  | Pfloatcomp(Cle) -> fprintf ppf "<=."
  | Pfloatcomp(Cgt) -> fprintf ppf ">."
  | Pfloatcomp(Cge) -> fprintf ppf ">=."
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringsetu -> fprintf ppf "string.unsafe_set"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pstringsets -> fprintf ppf "string.set"
  | Parraylength _ -> fprintf ppf "array.length"
  | Pmakearray _ -> fprintf ppf "makearray "
  | Parrayrefu _ -> fprintf ppf "array.unsafe_get"
  | Parraysetu _ -> fprintf ppf "array.unsafe_set"
  | Parrayrefs _ -> fprintf ppf "array.get"
  | Parraysets _ -> fprintf ppf "array.set"
  | Pctconst c ->
     let const_name = match c with
       | Big_endian -> "big_endian"
       | Word_size -> "word_size"
       | Ostype_unix -> "ostype_unix"
       | Ostype_win32 -> "ostype_win32"
       | Ostype_cygwin -> "ostype_cygwin" in
     fprintf ppf "sys.constant_%s" const_name
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pbittest -> fprintf ppf "testbit"
  | Pbintofint bi -> print_boxed_integer "of_int" ppf bi
  | Pintofbint bi -> print_boxed_integer "to_int" ppf bi
  | Pcvtbint (bi1, bi2) -> print_boxed_integer_conversion ppf bi1 bi2
  | Pnegbint bi -> print_boxed_integer "neg" ppf bi
  | Paddbint bi -> print_boxed_integer "add" ppf bi
  | Psubbint bi -> print_boxed_integer "sub" ppf bi
  | Pmulbint bi -> print_boxed_integer "mul" ppf bi
  | Pdivbint bi -> print_boxed_integer "div" ppf bi
  | Pmodbint bi -> print_boxed_integer "mod" ppf bi
  | Pandbint bi -> print_boxed_integer "and" ppf bi
  | Porbint bi -> print_boxed_integer "or" ppf bi
  | Pxorbint bi -> print_boxed_integer "xor" ppf bi
  | Plslbint bi -> print_boxed_integer "lsl" ppf bi
  | Plsrbint bi -> print_boxed_integer "lsr" ppf bi
  | Pasrbint bi -> print_boxed_integer "asr" ppf bi
  | Pbintcomp(bi, Ceq) -> print_boxed_integer "==" ppf bi
  | Pbintcomp(bi, Cneq) -> print_boxed_integer "!=" ppf bi
  | Pbintcomp(bi, Clt) -> print_boxed_integer "<" ppf bi
  | Pbintcomp(bi, Cgt) -> print_boxed_integer ">" ppf bi
  | Pbintcomp(bi, Cle) -> print_boxed_integer "<=" ppf bi
  | Pbintcomp(bi, Cge) -> print_boxed_integer ">=" ppf bi
  | Pbigarrayref(unsafe, n, kind, layout) ->
      print_bigarray "get" unsafe kind ppf layout
  | Pbigarrayset(unsafe, n, kind, layout) ->
      print_bigarray "set" unsafe kind ppf layout
  | Pbigarraydim(n) -> fprintf ppf "Bigarray.dim_%i" n
  | Pstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get16"
     else fprintf ppf "string.get16"
  | Pstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get32"
     else fprintf ppf "string.get32"
  | Pstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_get64"
     else fprintf ppf "string.get64"
  | Pstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set16"
     else fprintf ppf "string.set16"
  | Pstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set32"
     else fprintf ppf "string.set32"
  | Pstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "string.unsafe_set64"
     else fprintf ppf "string.set64"
  | Pbigstring_load_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get16"
     else fprintf ppf "bigarray.array1.get16"
  | Pbigstring_load_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get32"
     else fprintf ppf "bigarray.array1.get32"
  | Pbigstring_load_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_get64"
     else fprintf ppf "bigarray.array1.get64"
  | Pbigstring_set_16(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set16"
     else fprintf ppf "bigarray.array1.set16"
  | Pbigstring_set_32(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set32"
     else fprintf ppf "bigarray.array1.set32"
  | Pbigstring_set_64(unsafe) ->
     if unsafe then fprintf ppf "bigarray.array1.unsafe_set64"
     else fprintf ppf "bigarray.array1.set64"
  | Pbswap16 -> fprintf ppf "bswap16"
  | Pbbswap(bi) -> print_boxed_integer "bswap" ppf bi
  | Pint_as_pointer -> fprintf ppf "int_as_pointer"

let rec lam ppf = function
  | Lvar id ->
      Ident.print ppf id
  | Lconst cst ->
      struct_const ppf cst
  | Lapply(lfun, largs, _) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(apply@ %a%a)@]" lam lfun lams largs
  | Lfunction(kind, params, body) ->
      let pr_params ppf params =
        match kind with
        | Curried ->
            List.iter (fun param -> fprintf ppf "@ %a" Ident.print param) params
        | Tupled ->
            fprintf ppf " (";
            let first = ref true in
            List.iter
              (fun param ->
                if !first then first := false else fprintf ppf ",@ ";
                Ident.print ppf param)
              params;
            fprintf ppf ")" in
      fprintf ppf "@[<2>(function%a@ %a)@]" pr_params params lam body
  | Llet(str, id, arg, body) ->
      let kind = function
        Alias -> "a" | Strict -> "" | StrictOpt -> "o" | Variable -> "v" in
      let rec letbody = function
        | Llet(str, id, arg, body) ->
            fprintf ppf "@ @[<2>%a =%s@ %a@]" Ident.print id (kind str) lam arg;
            letbody body
        | expr -> expr in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a =%s@ %a@]"
        Ident.print id (kind str) lam arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec(id_arg_list, body) ->
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
          id_arg_list in
      fprintf ppf
        "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam body
  | Lprim(prim, largs) ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch(larg, sw) ->
      let switch ppf sw =
        let spc = ref false in
        List.iter
         (fun (n, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
         sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
          sw.sw_blocks ;
        begin match sw.sw_failaction with
        | None  -> ()
        | Some l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam l
        end in
      fprintf ppf
       "@[<1>(%s %a@ @[<v 0>%a@])@]"
       (match sw.sw_failaction with None -> "switch*" | _ -> "switch")
       lam larg switch sw
  | Lstringswitch(arg, cases, default) ->
      let switch ppf cases =
        let spc = ref false in
        List.iter
         (fun (s, l) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
          cases;
        begin match default with
        | Some default ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>default:@ %a@]" lam default
        | None -> ()
        end in
      fprintf ppf
       "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Lstaticraise (i, ls)  ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls;
  | Lstaticcatch(lbody, (i, vars), lhandler) ->
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
  | Ltrywith(lbody, param, lhandler) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]"
        lam lbody Ident.print param lam lhandler
  | Lifthenelse(lcond, lif, lelse) ->
      fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence(l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile(lcond, lbody) ->
      fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor(param, lo, hi, dir, body) ->
      fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]"
       Ident.print param lam lo
       (match dir with Upto -> "to" | Downto -> "downto")
       lam hi lam body
  | Lassign(id, expr) ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (k, met, obj, largs, _) ->
      let args ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
      let kind =
        if k = Self then "self" else if k = Cached then "cache" else "" in
      fprintf ppf "@[<2>(send%s@ %a@ %a%a)@]" kind lam obj lam met args largs
  | Levent(expr, ev) ->
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
  | Lifused(id, expr) ->
      fprintf ppf "@[<2>(ifused@ %a@ %a)@]" Ident.print id lam expr

and sequence ppf = function
  | Lsequence(l1, l2) ->
      fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l ->
      lam ppf l

let structured_constant = struct_const

let lambda = lam
