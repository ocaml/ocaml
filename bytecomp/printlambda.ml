(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format
open Asttypes
open Primitive
open Types
open Lambda


let rec structured_constant = function
    Const_base(Const_int n) -> print_int n
  | Const_base(Const_char c) ->
      print_string "'"; print_string(Char.escaped c); print_string "'"
  | Const_base(Const_string s) ->
      print_string "\""; print_string(String.escaped s); print_string "\""
  | Const_base(Const_float s) ->
      print_string s
  | Const_pointer n -> print_int n; print_string "a"
  | Const_block(tag, []) ->
      print_string "["; print_int tag; print_string "]"
  | Const_block(tag, sc1::scl) ->
      open_box 1;
      print_string "["; print_int tag; print_string ":";
      print_space();
      open_box 0;
      structured_constant sc1;
      List.iter (fun sc -> print_space(); structured_constant sc) scl;
      close_box();
      print_string "]";
      close_box()
  | Const_float_array [] ->
      print_string "[| |]"
  | Const_float_array (f1 :: fl) ->
      open_box 1;
      print_string "[|";
      open_box 0;
      print_string f1;
      List.iter (fun f -> print_space(); print_string f) fl;
      close_box();
      print_string "|]";
      close_box()

let primitive = function
    Pidentity -> print_string "id"
  | Pgetglobal id -> print_string "global "; Ident.print id
  | Psetglobal id -> print_string "setglobal "; Ident.print id
  | Pmakeblock(tag, Immutable) -> print_string "makeblock "; print_int tag
  | Pmakeblock(tag, Mutable) -> print_string "makemutable "; print_int tag
  | Pfield n -> print_string "field "; print_int n
  | Psetfield(n, _) -> print_string "setfield "; print_int n
  | Pfloatfield n -> print_string "floatfield "; print_int n
  | Psetfloatfield n -> print_string "setfloatfield "; print_int n
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
  | Pbittest -> print_string "testbit"

let rec lambda = function
    Lvar id ->
      Ident.print id
  | Lconst cst ->
      structured_constant cst
  | Lapply(lfun, largs) ->
      open_box 2;
      print_string "(apply"; print_space();
      lambda lfun;
      List.iter (fun l -> print_space(); lambda l) largs;
      print_string ")";
      close_box()
  | Lfunction(kind, params, body) ->
      open_box 2;
      print_string "(function";
      begin match kind with
        Curried ->
          List.iter (fun param -> print_space(); Ident.print param) params
      | Tupled ->
          print_string " (";
          let first = ref true in
          List.iter
            (fun param ->
              if !first
              then first := false
              else begin print_string ",";print_space() end;
              Ident.print param)
            params
      end;
      print_space(); lambda body; print_string ")"; close_box()
  | Llet(str, id, arg, body) ->
      open_box 2;
      print_string "(let"; print_space();
      open_hvbox 1;
      print_string "(";
      open_box 2; Ident.print id; print_space(); lambda arg; close_box();
      letbody body;
      print_string ")";
      close_box()
  | Lletrec(id_arg_list, body) ->
      open_box 2;
      print_string "(letrec"; print_space();
      print_string "(";
      open_hvbox 1;
      let spc = ref false in
      List.iter
        (fun (id, l) ->
          if !spc then print_space() else spc := true;
          open_box 2;
          Ident.print id; print_space(); lambda l;
          close_box())
        id_arg_list;
      close_box();
      print_string ")";
      print_space(); lambda body;
      print_string ")"; close_box()
  | Lprim(prim, largs) ->
      open_box 2;
      print_string "("; primitive prim;
      List.iter (fun l -> print_space(); lambda l) largs;
      print_string ")";
      close_box()
  | Lswitch(larg, sw) ->
      open_box 1;
      print_string (if sw.sw_checked then "(switch-checked " else "(switch ");
      lambda larg; print_space();
      open_vbox 0;
      let spc = ref false in
      List.iter
        (fun (n, l) ->
          if !spc then print_space() else spc := true;
          open_hvbox 1;
          print_string "case int "; print_int n;
          print_string ":"; print_space();
          lambda l;
          close_box())
        sw.sw_consts;
      List.iter
        (fun (n, l) ->
          if !spc then print_space() else spc := true;
          open_hvbox 1;
          print_string "case tag "; print_int n;
          print_string ":"; print_space();
          lambda l;
          close_box())
        sw.sw_blocks;
      print_string ")"; close_box(); close_box()
  | Lstaticfail ->
      print_string "exit"
  | Lcatch(lbody, lhandler) ->
      open_box 2;
      print_string "(catch"; print_space();
      lambda lbody; print_break 1 (-1);
      print_string "with"; print_space(); lambda lhandler;
      print_string ")";
      close_box()
  | Ltrywith(lbody, param, lhandler) ->
      open_box 2;
      print_string "(try"; print_space();
      lambda lbody; print_break 1 (-1);
      print_string "with "; Ident.print param; print_space();
      lambda lhandler;
      print_string ")";
      close_box()
  | Lifthenelse(lcond, lif, lelse) ->
      open_box 2;
      print_string "(if"; print_space();
      lambda lcond; print_space();
      lambda lif; print_space();
      lambda lelse; print_string ")";
      close_box()
  | Lsequence(l1, l2) ->
      open_box 2;
      print_string "(seq"; print_space();
      lambda l1; print_space(); sequence l2; print_string ")";
      close_box()
  | Lwhile(lcond, lbody) ->
      open_box 2;
      print_string "(while"; print_space();
      lambda lcond; print_space();
      lambda lbody; print_string ")";
      close_box()
  | Lfor(param, lo, hi, dir, body) ->
      open_box 2;
      print_string "(for "; Ident.print param; print_space();
      lambda lo; print_space();
      print_string(match dir with Upto -> "to" | Downto -> "downto");
      print_space();
      lambda hi; print_space();
      lambda body; print_string ")";
      close_box()
  | Lassign(id, expr) ->
      open_box 2;
      print_string "(assign"; print_space();
      Ident.print id; print_space();
      lambda expr; print_string ")";
      close_box()
  | Lsend (met, obj, largs) ->
      open_box 2;
      print_string "(send"; print_space();
      lambda obj; print_space();
      lambda met;
      List.iter (fun l -> print_space(); lambda l) largs;
      print_string ")";
      close_box()
  | Levent(lam, ev) ->
      open_box 2;
      begin match ev.lev_kind with
        Lev_before  -> print_string "(before "
      | Lev_after _ -> print_string "(after "
      end;
      print_int ev.lev_loc;
      print_space();
      lambda lam;
      print_string ")";
      close_box()

and sequence = function
    Lsequence(l1, l2) ->
      sequence l1; print_space(); sequence l2
  | l ->
      lambda l

and letbody = function
    Llet(str, id, arg, body) ->
      print_space();
      open_box 2; Ident.print id; print_space(); lambda arg;
      close_box();
      letbody body
  | l ->
      print_string ")";
      close_box();
      print_space();
      lambda l
