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

(* Pretty-printing of pseudo machine code *)

open Formatmsg
open Cmm
open Reg
open Mach

let register ppf r =
  if String.length r.name > 0 then
    print_string r.name
  else
    print_string(match r.typ with Addr -> "A" | Int -> "I" | Float -> "F");
  printf "/%i" r.stamp;
  begin match r.loc with
    Unknown -> ()
  | Reg r -> 
      printf "[%s]" (Proc.register_name r)
  | Stack(Local s) ->
      printf "[s%i]" s
  | Stack(Incoming s) ->
      printf "[si%i]" s
  | Stack(Outgoing s) ->
      printf "[so%i]" s
  end

let reg r = printf "%a" register r

let regs v =
  match Array.length v with
    0 -> ()
  | 1 -> reg v.(0)
  | n -> reg v.(0);
         for i = 1 to n-1 do print_string " "; reg v.(i) done

let regset s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then first := false else print_space();
      reg r)
    s

let regsetaddr s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then first := false else print_space();
      reg r;
      match r.typ with Addr -> print_string "*" | _ -> ())
    s

let intcomp = function
    Isigned c -> print_string " "; Printcmm.comparison c; print_string "s "
  | Iunsigned c -> print_string " "; Printcmm.comparison c; print_string "u "

let floatcomp c =
    print_string " "; Printcmm.comparison c; print_string "f "

let intop = function
    Iadd -> print_string " + "
  | Isub -> print_string " - "
  | Imul -> print_string " * "
  | Idiv -> print_string " div "
  | Imod -> print_string " mod "
  | Iand -> print_string " & "
  | Ior -> print_string " | "
  | Ixor -> print_string " ^ "
  | Ilsl -> print_string " << "
  | Ilsr -> print_string " >>u "
  | Iasr -> print_string " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound -> print_string " check > "
    
let test tst arg =
  match tst with
    Itruetest -> reg arg.(0)
  | Ifalsetest -> print_string "not "; reg arg.(0)
  | Iinttest cmp -> reg arg.(0); intcomp cmp; reg arg.(1)
  | Iinttest_imm(cmp, n) -> reg arg.(0); intcomp cmp; print_int n
  | Ifloattest(cmp, neg) ->
      if neg then print_string "not ";
      reg arg.(0); floatcomp cmp; reg arg.(1)
  | Ieventest -> reg arg.(0); print_string " & 1 == 0"
  | Ioddtest -> reg arg.(0); print_string " & 1 == 1"

let print_live = ref false

let operation op arg res =
  if Array.length res > 0 then begin regs res; print_string " := " end;
  match op with
    Imove -> regs arg
  | Ispill -> regs arg; print_string " (spill)"
  | Ireload -> regs arg; print_string " (reload)"
  | Iconst_int n -> print_string(Nativeint.to_string n)
  | Iconst_float s -> print_string s
  | Iconst_symbol s -> printf "\"%s\"" s
  | Icall_ind -> print_string "call "; regs arg
  | Icall_imm lbl ->
      printf "call \"%s\" " lbl;
      regs arg
  | Itailcall_ind -> print_string "tailcall "; regs arg
  | Itailcall_imm lbl ->
      printf "tailcall \"%s\" " lbl;
      regs arg
  | Iextcall(lbl, alloc) ->
      printf "extcall \"%s\" " lbl;
      regs arg;
      if not alloc then print_string " (noalloc)"
  | Istackoffset n ->
      printf "offset stack %i" n
  | Iload(chunk, addr) ->
      Printcmm.chunk chunk;
      print_string "[";
      Arch.print_addressing reg addr arg;
      print_string "]"
  | Istore(chunk, addr) ->
      Printcmm.chunk chunk;
      print_string "[";
      Arch.print_addressing reg addr (Array.sub arg 1 (Array.length arg - 1));
      print_string "] := ";
      reg arg.(0)
  | Ialloc n -> printf "alloc %i" n
  | Iintop(op) -> reg arg.(0); intop op; reg arg.(1)
  | Iintop_imm(op, n) -> reg arg.(0); intop op; print_int n
  | Inegf -> print_string "-f "; reg arg.(0)
  | Iabsf -> print_string "absf "; reg arg.(0)
  | Iaddf -> reg arg.(0); print_string " +f "; reg arg.(1)
  | Isubf -> reg arg.(0); print_string " -f "; reg arg.(1)
  | Imulf -> reg arg.(0); print_string " *f "; reg arg.(1)
  | Idivf -> reg arg.(0); print_string " /f "; reg arg.(1)
  | Ifloatofint -> print_string "floatofint "; reg arg.(0)
  | Iintoffloat -> print_string "intoffloat "; reg arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation reg op arg

let rec instruction ppf i =
  if !print_live then begin
    printf "@[<1>{";
    regsetaddr i.live;
    if Array.length i.arg > 0 then begin
      printf "@ +@ "; regs i.arg
    end;
    printf "}@]@,";
  end;
  begin match i.desc with
    Iend -> ()
  | Iop op ->
      operation op i.arg i.res
  | Ireturn ->
      print_string "return "; regs i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      printf "@[<v 2>if "; test tst i.arg;
      printf " then@,%a" instruction ifso;
      begin match ifnot.desc with
        Iend -> ()
      | _ -> printf "@;<0 -2>else@,%a" instruction ifnot
      end;
      printf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      printf "switch %a" register i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        printf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then printf "case %i:@," j
        done;
        printf "@]@,%a@]" instruction cases.(i)
      done;
      printf "@,endswitch"
  | Iloop(body) ->
      printf "@[<v 2>loop@,%a@;<0 -2>endloop@]" instruction body
  | Icatch(body, handler) ->
      printf "@[<v 2>catch@,%a@;<0 -2>with@,%a@;<0 -2>endcatch@]"
             instruction body instruction handler
  | Iexit ->
      print_string "exit"
  | Itrywith(body, handler) ->
      printf "@[<v 2>try@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             instruction body instruction handler
  | Iraise ->
      printf "raise %a" register i.arg.(0)
  end;
  begin match i.next.desc with
    Iend -> ()
  | _ -> printf "@,%a" instruction i.next
  end

let functiondecl ppf f =
  printf "@[<v 2>%s(" f.fun_name;
  regs f.fun_args;
  printf ")@,%a@]" instruction f.fun_body

let phase msg f =
  printf "*** %s@.%a@." msg functiondecl f

let interference r =
  let interf ppf =
   List.iter
    (fun r -> printf "@ %a" register r)
    r.interf in
  printf "@[<2>%a:%t@]@." register r interf

let interferences () =
  printf "*** Interferences@.";
  List.iter interference (Reg.all_registers())

let preference r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> printf "@ %a weight %i" register r w)
      r.prefer in
  printf "@[<2>%a: %t@]@." register r prefs

let preferences () =
  printf "*** Preferences@.";
  List.iter preference (Reg.all_registers())

let fundecl d = printf "%a" functiondecl d
let instr i = printf "%a" instruction i
