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

(* Pretty-printing of linearized machine code *)

open Formatmsg
open Mach
open Printmach
open Linearize

let label ppf l =
  Format.fprintf ppf "L%i" l

let instr i =
  match i.desc with
    Lend -> ()
  | Lop op ->
      begin match op with
        Ialloc _ | Icall_ind | Icall_imm _ | Iextcall(_, _) ->
          printf "@[<1>{";
          regsetaddr i.live;
          printf "}@]@,"
      | _ -> ()
      end;
      operation op i.arg i.res
  | Lreloadretaddr ->
      print_string "reload retaddr"
  | Lreturn ->
      print_string "return "; regs i.arg
  | Llabel lbl ->
      printf "%a:" label lbl
  | Lbranch lbl ->
      printf "goto %a" label lbl
  | Lcondbranch(tst, lbl) ->
      printf "if "; test tst i.arg; printf " goto %a" label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      print_string "switch3 "; reg i.arg.(0);
      let case n = function
        None -> ()
      | Some lbl ->
          printf "@,case %i: goto %a" n label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      printf "@,endswitch"
  | Lswitch lblv ->
      printf "switch "; reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
        printf "case %i: goto %a" i label lblv.(i)
      done;
      printf "@,endswitch"
  | Lsetuptrap lbl ->
      printf "setup trap %a" label lbl
  | Lpushtrap ->
      print_string "push trap"
  | Lpoptrap ->
      print_string "pop trap"
  | Lraise ->
      print_string "raise "; reg i.arg.(0)

let rec all_instr ppf i =
  match i.desc with
    Lend -> ()
  | _ -> instr i; printf "@,%a" all_instr i.next

let fundecl f =
  printf "@[<v 2>%s:@,%a@]" f.fun_name all_instr f.fun_body
