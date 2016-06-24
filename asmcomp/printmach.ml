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

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach

let reg register_name ppf r =
  if not (Reg.anonymous r) then
    fprintf ppf "%s" (Reg.name r)
  else
    fprintf ppf "%s"
      (match r.typ with Val -> "V" | Addr -> "A" | Int -> "I" | Float -> "F");
  fprintf ppf "/%i" r.stamp;
  begin match r.loc with
  | Unknown -> ()
  | Reg r ->
      fprintf ppf "[%s]" (register_name r)
  | Stack(Local s) ->
      fprintf ppf "[s%i]" s
  | Stack(Incoming s) ->
      fprintf ppf "[si%i]" s
  | Stack(Outgoing s) ->
      fprintf ppf "[so%i]" s
  end

let regs register_name ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> reg register_name ppf v.(0)
  | n -> reg register_name ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" (reg register_name) v.(i) done

let regset register_name ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" (reg register_name) r end
      else fprintf ppf "@ %a" (reg register_name) r)
    s

let regsetaddr register_name ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" (reg register_name) r end
      else fprintf ppf "@ %a" (reg register_name) r;
      match r.typ with
      | Val -> fprintf ppf "*"
      | Addr -> fprintf ppf "!"
      | _ -> ())
    s

let intcomp = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (Printcmm.comparison c)

let intop = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior ->  " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound -> " check > "

let test register_name tst ppf arg =
  match tst with
  | Itruetest -> reg register_name ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" (reg register_name) arg.(0)
  | Iinttest cmp -> fprintf ppf "%a%s%a" (reg register_name) arg.(0) (intcomp cmp) (reg register_name) arg.(1)
  | Iinttest_imm(cmp, n) -> fprintf ppf "%a%s%i" (reg register_name) arg.(0) (intcomp cmp) n
  | Ifloattest(cmp, neg) ->
      fprintf ppf "%s%a%s%a"
       (if neg then "not " else "")
       (reg register_name) arg.(0) (floatcomp cmp) (reg register_name) arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" (reg register_name) arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" (reg register_name) arg.(0)

let print_live = ref false

let operation register_name print_addressing print_specific_operation op arg ppf res =
  if Array.length res > 0 then fprintf ppf "%a := " (regs register_name) res;
  match op with
  | Imove -> regs register_name ppf arg
  | Ispill -> fprintf ppf "%a (spill)" (regs register_name) arg
  | Ireload -> fprintf ppf "%a (reload)" (regs register_name) arg
  | Iconst_int n
  | Iconst_blockheader n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Iconst_float f -> fprintf ppf "%F" (Int64.float_of_bits f)
  | Iconst_symbol s -> fprintf ppf "\"%s\"" s
  | Icall_ind -> fprintf ppf "call %a" (regs register_name) arg
  | Icall_imm lbl -> fprintf ppf "call \"%s\" %a" lbl (regs register_name) arg
  | Itailcall_ind -> fprintf ppf "tailcall %a" (regs register_name) arg
  | Itailcall_imm lbl -> fprintf ppf "tailcall \"%s\" %a" lbl (regs register_name) arg
  | Iextcall(lbl, alloc) ->
      fprintf ppf "extcall \"%s\" %a%s" lbl (regs register_name) arg
      (if alloc then "" else " (noalloc)")
  | Istackoffset n ->
      fprintf ppf "offset stack %i" n
  | Iload(chunk, addr) ->
      fprintf ppf "%s[%a]"
       (Printcmm.chunk chunk) (print_addressing (reg register_name) addr) arg
  | Istore(chunk, addr, is_assign) ->
      fprintf ppf "%s[%a] := %a %s"
       (Printcmm.chunk chunk)
       (print_addressing (reg register_name) addr)
       (Array.sub arg 1 (Array.length arg - 1))
       (reg register_name) arg.(0)
       (if is_assign then "(assign)" else "(init)")
  | Ialloc n -> fprintf ppf "alloc %i" n
  | Iintop(op) -> fprintf ppf "%a%s%a" (reg register_name) arg.(0) (intop op) (reg register_name) arg.(1)
  | Iintop_imm(op, n) -> fprintf ppf "%a%s%i" (reg register_name) arg.(0) (intop op) n
  | Inegf -> fprintf ppf "-f %a" (reg register_name) arg.(0)
  | Iabsf -> fprintf ppf "absf %a" (reg register_name) arg.(0)
  | Iaddf -> fprintf ppf "%a +f %a" (reg register_name) arg.(0) (reg register_name) arg.(1)
  | Isubf -> fprintf ppf "%a -f %a" (reg register_name) arg.(0) (reg register_name) arg.(1)
  | Imulf -> fprintf ppf "%a *f %a" (reg register_name) arg.(0) (reg register_name) arg.(1)
  | Idivf -> fprintf ppf "%a /f %a" (reg register_name) arg.(0) (reg register_name) arg.(1)
  | Ifloatofint -> fprintf ppf "floatofint %a" (reg register_name) arg.(0)
  | Iintoffloat -> fprintf ppf "intoffloat %a" (reg register_name) arg.(0)
  | Ispecific op ->
      print_specific_operation (reg register_name) op ppf arg

let rec instr register_name print_addressing print_specific_operation ppf i =
  let instr = instr register_name print_addressing print_specific_operation in
  if !print_live then begin
    fprintf ppf "@[<1>{%a" (regsetaddr register_name) i.live;
    if Array.length i.arg > 0 then fprintf ppf "@ +@ %a" (regs register_name) i.arg;
    fprintf ppf "}@]@,";
  end;
  begin match i.desc with
  | Iend -> ()
  | Iop op ->
      operation register_name print_addressing print_specific_operation op i.arg ppf i.res
  | Ireturn ->
      fprintf ppf "return %a" (regs register_name) i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      fprintf ppf "@[<v 2>if %a then@,%a" (test register_name tst) i.arg instr ifso;
      begin match ifnot.desc with
      | Iend -> ()
      | _ -> fprintf ppf "@;<0 -2>else@,%a" instr ifnot
      end;
      fprintf ppf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      fprintf ppf "switch %a" (reg register_name) i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        fprintf ppf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:@," j
        done;
        fprintf ppf "@]@,%a@]" instr cases.(i)
      done;
      fprintf ppf "@,endswitch"
  | Iloop(body) ->
      fprintf ppf "@[<v 2>loop@,%a@;<0 -2>endloop@]" instr body
  | Icatch(i, body, handler) ->
      fprintf
        ppf "@[<v 2>catch@,%a@;<0 -2>with(%d)@,%a@;<0 -2>endcatch@]"
        instr body i instr handler
  | Iexit i ->
      fprintf ppf "exit(%d)" i
  | Itrywith(body, handler) ->
      fprintf ppf "@[<v 2>try@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             instr body instr handler
  | Iraise k ->
      fprintf ppf "%s %a" (Lambda.raise_kind k) (reg register_name) i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) then
    fprintf ppf "%s" (Debuginfo.to_string i.dbg);
  begin match i.next.desc with
    Iend -> ()
  | _ -> fprintf ppf "@,%a" instr i.next
  end

let fundecl register_name print_addressing print_specific_operation ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s(%a)%s@,%a@]"
    f.fun_name (regs register_name) f.fun_args dbg (instr register_name print_addressing print_specific_operation) f.fun_body

let phase msg register_name print_addressing print_specific_operation ppf f =
  fprintf ppf "*** %s@.%a@." msg (fundecl register_name print_addressing print_specific_operation) f

let interference register_name ppf r =
  let interf ppf =
   List.iter
    (fun r -> fprintf ppf "@ %a" (reg register_name) r)
    r.interf in
  fprintf ppf "@[<2>%a:%t@]@." (reg register_name) r interf

let interferences register_name ppf () =
  fprintf ppf "*** Interferences@.";
  List.iter (interference register_name ppf) (Reg.all_registers())

let preference register_name ppf r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> fprintf ppf "@ %a weight %i" (reg register_name) r w)
      r.prefer in
  fprintf ppf "@[<2>%a: %t@]@." (reg register_name) r prefs

let preferences register_name ppf () =
  fprintf ppf "*** Preferences@.";
  List.iter (preference register_name ppf) (Reg.all_registers())
