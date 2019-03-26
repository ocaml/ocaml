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

(* Pretty-printing of linearized machine code *)

open Format
open Mach
open Printmach
open Linearize

module S = Backend_sym

include Ir_debug.Make_mark_functions (struct
  include Insn_debuginfo
  let position t = linear_position t
end)

module Mark_fun_dbg = Ir_debug.Make_mark_functions (struct
  include Debuginfo.Function
  let position t = Some (position t)
end)

let label ppf l =
  Format.fprintf ppf "L%i" l

let instr ?no_debuginfo ppf i =
  mark_start_location ppf i.dbg;
  begin match i.desc with
  | Lend -> ()
  | Lprologue ->
      fprintf ppf "prologue"
  | Lop op ->
      begin match op with
      | Ialloc _ | Icall_ind _ | Icall_imm _ | Iextcall _ ->
          fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
      | _ -> ()
      end;
      operation op i.arg ppf i.res
  | Lreloadretaddr ->
      fprintf ppf "reload retaddr"
  | Lreturn ->
      fprintf ppf "return %a" regs i.arg
  | Llabel lbl ->
      fprintf ppf "%a:" label lbl
  | Lbranch lbl ->
      fprintf ppf "goto %a" label lbl
  | Lcondbranch(tst, lbl) ->
      fprintf ppf "if %a goto %a" (test tst) i.arg label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      fprintf ppf "switch3 %a" reg i.arg.(0);
      let case n = function
      | None -> ()
      | Some lbl ->
         fprintf ppf "@,case %i: goto %a" n label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      fprintf ppf "@,endswitch"
  | Lswitch lblv ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
       fprintf ppf "case %i: goto %a" i label lblv.(i)
      done;
      fprintf ppf "@,endswitch"
  | Lsetuptrap lbl ->
      fprintf ppf "setup trap %a" label lbl
  | Lpushtrap ->
      fprintf ppf "push trap"
  | Lpoptrap ->
      fprintf ppf "pop trap"
  | Lraise k ->
      fprintf ppf "%a %a" Printcmm.raise_kind k reg i.arg.(0)
  end;
  mark_end_location ppf i.dbg;
  match no_debuginfo with
  | Some () -> ()
  | None ->
      let phantom_available_before =
        Insn_debuginfo.phantom_available_before i.dbg 
      in
      let available_before = Insn_debuginfo.available_before i.dbg in
      let available_across = Insn_debuginfo.available_across i.dbg in
      let dbg = Insn_debuginfo.dbg i.dbg in
      (* CR mshinwell: Use a printer in [Insn_debuginfo] *)
      if !Clflags.dump_avail && !Clflags.dump_linear_dbg then begin
        let print_reg = Printmach.reg in
        if not (Backend_var.Set.is_empty phantom_available_before) then
          fprintf ppf "@;@{<debug_avail>@[<hov 4>^---(available_before@ %a)@ \
              (phantom_available_before@ %a)@ \
              (available_across@ %a)@]@}"
            (Reg_availability_set.print ~print_reg) available_before
            Backend_var.Set.print phantom_available_before
            (Misc.Stdlib.Option.print (Reg_availability_set.print ~print_reg))
            available_across
        else
          fprintf ppf "@;@{<debug_avail>@[<hov 4>^---(available_before@ %a)@ \
              (available_across@ %a)@]@}"
            (Reg_availability_set.print ~print_reg) available_before
            (Misc.Stdlib.Option.print (Reg_availability_set.print ~print_reg))
            available_across
      end;
      if (not (Debuginfo.is_none dbg)) && !Clflags.dump_linear_dbg then begin
        fprintf ppf "@ @[<hov 4>@{<debug_loc>    %a@}@]" Debuginfo.print dbg
      end

let rec all_instr ?no_debuginfo ppf i =
  match i.desc with
  | Lend -> ()
  | _ ->
      fprintf ppf "%a@,%a"
        (instr ?no_debuginfo) i
        (all_instr ?no_debuginfo) i.next

let fundecl ?no_debuginfo ppf f =
  Mark_fun_dbg.mark_start_location ppf f.fun_dbg;
  let dbg =
    if not !Clflags.dump_linear_dbg then ""
    else Format.asprintf " %a" Debuginfo.Function.print f.fun_dbg in
  fprintf ppf "@[<v 2>%a:%s@,%a@]" S.print f.fun_name dbg
    (all_instr ?no_debuginfo) f.fun_body;
  Mark_fun_dbg.mark_end_location ppf f.fun_dbg
