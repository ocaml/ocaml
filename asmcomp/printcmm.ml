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

(* Pretty-printing of C-- code *)

open Formatmsg
open Cmm

let machtype_component = function
    Addr -> print_string "addr"
  | Int -> print_string "int"
  | Float -> print_string "float"

let machtype mty =
  match Array.length mty with
    0 -> print_string "unit"
  | n -> machtype_component mty.(0);
         for i = 1 to n-1 do
           print_string "*"; machtype_component mty.(i)
         done

let comparison = function
    Ceq -> print_string "=="
  | Cne -> print_string "!="
  | Clt -> print_string "<"
  | Cle -> print_string "<="
  | Cgt -> print_string ">"
  | Cge -> print_string ">="

let chunk = function
    Byte_unsigned -> print_string "unsigned int8"
  | Byte_signed -> print_string "signed int8"
  | Sixteen_unsigned -> print_string "unsigned int16"
  | Sixteen_signed -> print_string "signed int16"
  | Thirtytwo_unsigned -> print_string "unsigned int32"
  | Thirtytwo_signed -> print_string "signed int32"
  | Word -> ()
  | Single -> print_string "float32"
  | Double -> print_string "float64"

let operation = function
    Capply ty -> print_string "app"
  | Cextcall(lbl, ty, alloc) -> printf "extcall \"%s\"" lbl
  | Cload Word -> print_string "load"
  | Cload c -> print_string "load "; chunk c
  | Calloc -> print_string "alloc"
  | Cstore Word -> print_string "store"
  | Cstore c -> print_string "store "; chunk c
  | Caddi -> print_string "+"
  | Csubi -> print_string "-"
  | Cmuli -> print_string "*"
  | Cdivi -> print_string "/"
  | Cmodi -> print_string "mod"
  | Cand -> print_string "and"
  | Cor -> print_string "or"
  | Cxor -> print_string "xor"
  | Clsl -> print_string "<<"
  | Clsr -> print_string ">>u"
  | Casr -> print_string ">>s"
  | Ccmpi c -> comparison c
  | Cadda -> print_string "+a"
  | Csuba -> print_string "-a"
  | Ccmpa c -> comparison c; print_string "a"
  | Cnegf -> print_string "~f"
  | Cabsf -> print_string "absf"
  | Caddf -> print_string "+f"
  | Csubf -> print_string "-f"
  | Cmulf -> print_string "*f"
  | Cdivf -> print_string "/f"
  | Cfloatofint -> print_string "floatofint"
  | Cintoffloat -> print_string "intoffloat"
  | Ccmpf c -> comparison c; print_string "f"
  | Craise -> print_string "raise"
  | Ccheckbound -> print_string "checkbound"

let print_id ppf id = Ident.print id;;

let rec expr ppf = function
    Cconst_int n -> print_int n
  | Cconst_natint n -> print_string(Nativeint.to_string n)
  | Cconst_float s -> print_string s
  | Cconst_symbol s -> printf "\"%s\"" s
  | Cconst_pointer n -> printf "%ia" n
  | Cvar id -> Ident.print id
  | Clet(id, def, (Clet(_, _, _) as body)) ->
      let print_binding id ppf def =
        printf "@[<2>%a@ %a@]" print_id id expr def in
      let rec in_part ppf = function
        | Clet(id, def, body) ->
            printf "@ %a" (print_binding id) def;
            in_part ppf body
        | exp -> exp in
      printf "@[<2>(let@ @[<1>(%a" (print_binding id) def;
      let exp = in_part ppf body in
      printf ")@]@ %a)@]" sequence exp
  | Clet(id, def, body) ->
     printf "@[<2>(let@ @[<2>%a@ %a@]@ %a)@]" print_id id expr def sequence body
  | Cassign(id, exp) ->
      printf "@[<2>(assign @[<2>%a@ %a@])@]" print_id id expr exp
  | Ctuple el ->
      let tuple ppf el =
       let first = ref true in
       List.iter
        (fun e ->
          if !first then first := false else print_space();
          expr ppf e)
        el in
      printf "@[<1>[%a]@]" tuple el
  | Cop(op, el) ->
      printf "@[<2>(";
      operation op;
      List.iter (fun e -> printf "@ %a" expr e) el;
      begin match op with
        Capply mty -> print_space(); machtype mty
      | Cextcall(_, mty, _) -> print_space(); machtype mty
      | _ -> ()
      end;
      printf ")@]"
  | Csequence(e1, e2) ->
      printf "@[<2>(seq@ %a@ %a)@]" sequence e1 sequence e2
  | Cifthenelse(e1, e2, e3) ->
      printf "@[<2>(if@ %a@ %a@ %a)@]" expr e1 expr e2 expr e3
  | Cswitch(e1, index, cases) ->
      let print_case i ppf =
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then printf "case %i:" j
        done in
      let print_cases ppf =
       for i = 0 to Array.length cases - 1 do
        printf "@ @[<2>%t@ %a@]" (print_case i) sequence cases.(i)
       done in
      printf "@[<v 0>@[<2>(switch@ %a@ @]%t)@]" expr e1 print_cases 
  | Cloop e ->
      printf "@[<2>(loop@ %a)@]" sequence e
  | Ccatch(e1, e2) ->
      printf "@[<2>(catch@ %a@;<1 -2>with@ %a)@]" sequence e1 sequence e2
  | Cexit ->
      print_string "exit"
  | Ctrywith(e1, id, e2) ->
      printf "@[<2>(try@ %a@;<1 -2>with@ %a@ %a)@]"
             sequence e1 print_id id sequence e2

and sequence ppf = function
    Csequence(e1, e2) ->
      printf "%a@ %a" sequence e1 sequence e2
  | e ->
      expression e

and expression e = printf "%a" expr e

let fundecl f =
  let print_cases ppf cases =
    let first = ref true in
    List.iter
     (fun (id, ty) -> 
       if !first then first := false else print_space();
       printf "%a: " print_id id;
       machtype ty)
     cases in
  printf "@[<1>(function %s@;<1 4>@[<1>(%a)@]@ @[%a@])@]@."
         f.fun_name print_cases f.fun_args sequence f.fun_body

let data_item = function
    Cdefine_symbol s -> printf "\"%s\":" s
  | Cdefine_label l -> printf "L%i:" l
  | Cint8 n -> printf "byte %i" n
  | Cint16 n -> printf "int16 %i" n
  | Cint32 n -> printf "int32 %s" (Nativeint.to_string n)
  | Cint n -> printf "int %s" (Nativeint.to_string n)
  | Csingle f -> printf "single %s" f
  | Cdouble f -> printf "double %s" f
  | Csymbol_address s -> printf "addr \"%s\"" s
  | Clabel_address l -> printf "addr L%i" l
  | Cstring s -> printf "string \"%s\"" s
  | Cskip n -> printf "skip %i" n
  | Calign n -> printf "align %i" n

let data dl =
  let items ppf = List.iter (fun d -> print_space(); data_item d) dl in
  printf "@[<hv 1>(data%t)@]" items

let phrase = function
    Cfunction f -> fundecl f
  | Cdata dl -> data dl
