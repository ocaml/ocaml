(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach

let reg r =
  if String.length r.name > 0 then
    print_string r.name
  else
    print_string(match r.typ with Addr -> "A" | Int -> "I" | Float -> "F");
  print_string "/";
  print_int r.stamp;
  begin match r.loc with
    Unknown -> ()
  | Reg r -> 
      print_string "["; print_string(Proc.register_name r); print_string "]"
  | Stack(Local s) ->
      print_string "[s"; print_int s; print_string "]"
  | Stack(Incoming s) ->
      print_string "[si"; print_int s; print_string "]"
  | Stack(Outgoing s) ->
      print_string "[so"; print_int s; print_string "]"
  end

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
  | Ifloattest cmp -> reg arg.(0); floatcomp cmp; reg arg.(1)
  | Ieventest -> reg arg.(0); print_string " & 1 == 0"
  | Ioddtest -> reg arg.(0); print_string " & 1 == 1"

let print_live = ref false

let operation op arg res =
  if Array.length res > 0 then begin regs res; print_string " := " end;
  match op with
    Imove -> regs arg
  | Ispill -> regs arg; print_string " (spill)"
  | Ireload -> regs arg; print_string " (reload)"
  | Iconst_int n -> print_int n
  | Iconst_float s -> print_string s
  | Iconst_symbol s -> print_string "\""; print_string s; print_string "\""
  | Icall_ind -> print_string "call "; regs arg
  | Icall_imm lbl ->
      print_string "call \""; print_string lbl;
      print_string "\" "; regs arg
  | Itailcall_ind -> print_string "tailcall "; regs arg
  | Itailcall_imm lbl ->
      print_string "tailcall \""; print_string lbl;
      print_string "\" "; regs arg
  | Iextcall(lbl, alloc) ->
      print_string "extcall \""; print_string lbl;
      print_string "\" "; regs arg;
      if not alloc then print_string " (noalloc)"
  | Istackoffset n ->
      print_string "offset stack "; print_int n
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
  | Ialloc n -> print_string "alloc "; print_int n
  | Imodify -> print_string "modify "; reg arg.(0)
  | Iintop(op) -> reg arg.(0); intop op; reg arg.(1)
  | Iintop_imm(op, n) -> reg arg.(0); intop op; print_int n
  | Iaddf -> reg arg.(0); print_string " +f "; reg arg.(1)
  | Isubf -> reg arg.(0); print_string " -f "; reg arg.(1)
  | Imulf -> reg arg.(0); print_string " *f "; reg arg.(1)
  | Idivf -> reg arg.(0); print_string " /f "; reg arg.(1)
  | Ifloatofint -> print_string "floatofint "; reg arg.(0)
  | Iintoffloat -> print_string "intoffloat "; reg arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation reg op arg

let rec instr i =
  if !print_live then begin
    open_hovbox 1;
    print_string "{";
    regsetaddr i.live;
    if Array.length i.arg > 0 then begin
      print_space(); print_string "+"; print_space(); regs i.arg
    end;
    print_string "}";
    close_box();
    print_cut()
  end;
  begin match i.desc with
    Iend -> ()
  | Iop op ->
      operation op i.arg i.res
  | Ireturn ->
      print_string "return "; regs i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      open_vbox 2;
      print_string "if "; test tst i.arg; print_string " then"; print_cut();
      instr ifso;
      begin match ifnot.desc with
        Iend -> ()
      | _ -> print_break 0 (-2); print_string "else"; print_cut(); instr ifnot
      end;
      print_break 0 (-2); print_string "endif";
      close_box()
  | Iswitch(index, cases) ->
      print_string "switch "; reg i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        print_cut();
        open_vbox 2;
        open_hovbox 0;
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then begin
            print_string "case "; print_int j; print_string ":";
            print_cut()
          end
        done;
        close_box(); print_cut();
        instr cases.(i);
        close_box()
      done;
      print_cut(); print_string "endswitch"
  | Iloop(body) ->
      open_vbox 2;
      print_string "loop"; print_cut();
      instr body; print_break 0 (-2); 
      print_string "endloop ";
      close_box()
  | Icatch(body, handler) ->
      open_vbox 2;
      print_string "catch"; print_cut();
      instr body;
      print_break 0 (-2);  print_string "with"; print_cut();
      instr handler;
      print_break 0 (-2); print_string "endcatch";
      close_box()
  | Iexit ->
      print_string "exit"
  | Itrywith(body, handler) ->
      open_vbox 2;
      print_string "try"; print_cut();
      instr body;
      print_break 0 (-2);  print_string "with"; print_cut();
      instr handler;
      print_break 0 (-2); print_string "endtry";
      close_box()
  | Iraise ->
      print_string "raise "; reg i.arg.(0)
  end;
  begin match i.next.desc with
    Iend -> ()
  | _ -> print_cut(); instr i.next
  end

let fundecl f =
  open_vbox 2;
  print_string f.fun_name;
  print_string "("; regs f.fun_args; print_string ")";
  print_cut();
  instr f.fun_body;
  close_box()

let phase msg f =
  print_string "*** "; print_string msg; print_newline(); 
  fundecl f; print_newline()

let interference r =
  open_hovbox 2;
  reg r; print_string ":";
  List.iter
    (fun r -> print_space(); reg r)
    r.interf;
  close_box();
  print_newline()

let interferences () =
  print_string "*** Interferences"; print_newline();
  List.iter interference (Reg.all_registers())

let preference r =
  open_hovbox 2;
  reg r; print_string ": ";
  List.iter
    (fun (r, w) -> print_space(); reg r; print_string " weight " ; print_int w)
    r.prefer;
  close_box();
  print_newline()

let preferences () =
  print_string "*** Preferences"; print_newline();
  List.iter preference (Reg.all_registers())
