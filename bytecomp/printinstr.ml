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

(* Pretty-print lists of instructions *)

open Formatmsg
open Lambda
open Instruct


let instruct ppf = function
    Klabel lbl -> printf "L%i:" lbl
  | Kacc n -> printf "\tacc %i" n
  | Kenvacc n -> printf "\tenvacc %i" n
  | Kpush -> print_string "\tpush"
  | Kpop n -> printf "\tpop %i" n
  | Kassign n -> printf "\tassign %i" n
  | Kpush_retaddr lbl -> printf "\tpush_retaddr L%i" lbl
  | Kapply n -> printf "\tapply %i" n
  | Kappterm(n, m) ->
      printf "\tappterm %i, %i" n m
  | Kreturn n -> printf "\treturn %i" n
  | Krestart -> print_string "\trestart"
  | Kgrab n -> printf "\tgrab %i" n
  | Kclosure(lbl, n) ->
      printf "\tclosure L%i, %i" lbl n
  | Kclosurerec(lbls, n) ->
      print_string "\tclosurerec";
      List.iter (fun lbl -> printf " %i" lbl) lbls;
      printf ", %i" n
  | Koffsetclosure n -> printf "\toffsetclosure %i" n
  | Kgetglobal id -> print_string "\tgetglobal "; Ident.print id
  | Ksetglobal id -> print_string "\tsetglobal "; Ident.print id
  | Kconst cst ->
      let pr_constant ppf cst = Printlambda.structured_constant cst in
      printf "@[<10>\tconst@ %a@]" pr_constant cst
  | Kmakeblock(n, m) ->
      printf "\tmakeblock %i, %i" n m
  | Kmakefloatblock(n) ->
      printf "\tmakefloatblock %i" n
  | Kgetfield n -> printf "\tgetfield %i" n
  | Ksetfield n -> printf "\tsetfield %i" n
  | Kgetfloatfield n -> printf "\tgetfloatfield %i" n
  | Ksetfloatfield n -> printf "\tsetfloatfield %i" n
  | Kvectlength -> print_string "\tvectlength"
  | Kgetvectitem -> print_string "\tgetvectitem"
  | Ksetvectitem -> print_string "\tsetvectitem"
  | Kgetstringchar -> print_string "\tgetstringchar"
  | Ksetstringchar -> print_string "\tsetstringchar"
  | Kbranch lbl -> printf "\tbranch L%i" lbl
  | Kbranchif lbl -> printf "\tbranchif L%i" lbl
  | Kbranchifnot lbl -> printf "\tbranchifnot L%i" lbl
  | Kstrictbranchif lbl -> printf "\tstrictbranchif L%i" lbl
  | Kstrictbranchifnot lbl ->
      printf "\tstrictbranchifnot L%i" lbl
  | Kswitch(consts, blocks) ->
      let labels ppf labs =
        Array.iter (fun lbl -> printf "@ %i" lbl) labs in
      printf "@[<10>\tswitch%a/%a@]" labels consts labels blocks
  | Kboolnot -> print_string "\tboolnot"
  | Kpushtrap lbl -> printf "\tpushtrap L%i" lbl
  | Kpoptrap -> print_string "\tpoptrap"
  | Kraise -> print_string "\traise"
  | Kcheck_signals -> print_string "\tcheck_signals"
  | Kccall(s, n) ->
      printf "\tccall %s, %i" s n
  | Knegint -> print_string "\tnegint"
  | Kaddint -> print_string "\taddint"
  | Ksubint -> print_string "\tsubint"
  | Kmulint -> print_string "\tmulint"
  | Kdivint -> print_string "\tdivint"
  | Kmodint -> print_string "\tmodint"
  | Kandint -> print_string "\tandint"
  | Korint -> print_string "\torint"
  | Kxorint -> print_string "\txorint"
  | Klslint -> print_string "\tlslint"
  | Klsrint -> print_string "\tlsrint"
  | Kasrint -> print_string "\tasrint"
  | Kintcomp Ceq -> print_string "\teqint"
  | Kintcomp Cneq -> print_string "\tneqint"
  | Kintcomp Clt -> print_string "\tltint"
  | Kintcomp Cgt -> print_string "\tgtint"
  | Kintcomp Cle -> print_string "\tleint"
  | Kintcomp Cge -> print_string "\tgeint"
  | Koffsetint n -> printf "\toffsetint %i" n
  | Koffsetref n -> printf "\toffsetref %i" n
  | Kisint -> print_string "\tisint"
  | Kgetmethod -> print_string "\tgetmethod"
  | Kstop -> print_string "\tstop"
  | Kevent ev -> printf "\tevent %i" ev.ev_char

let rec instruction_list ppf = function
    [] -> ()
  | Klabel lbl :: il ->
      printf "L%i:%a" lbl instruction_list il
  | instr :: il ->
      printf "%a@ %a" instruct instr instruction_list il
 
let instrlist il =
  printf "@[<v 0>%a@]" instruction_list il

let instruction i = printf "%a" instruct i
