(* Pretty-printing of linearized machine code *)

open Format
open Printmach
open Linearize

let label l =
  print_string "L"; print_int l

let instr i =
  match i.desc with
    Lend -> ()
  | Lop op ->
      operation op i.arg i.res
  | Lreturn ->
      print_string "return "; regs i.arg
  | Llabel lbl ->
      label lbl; print_string ":"
  | Lbranch lbl ->
      print_string "goto "; label lbl
  | Lcondbranch(tst, lbl) ->
      print_string "if "; test tst i.arg; print_string " goto "; label lbl
  | Lswitch lblv ->
      print_string "switch "; reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
        print_cut();
        print_string "case "; print_int i;
        print_string ": goto "; label lblv.(i)
      done;
      print_cut(); print_string "endswitch"
  | Lpushtrap lbl ->
      print_string "push trap "; label lbl
  | Lpoptrap ->
      print_string "pop trap"
  | Lentertrap ->
      print_string "enter trap"
  | Lraise ->
      print_string "raise "; reg i.arg.(0)

let rec all_instr i =
  match i.desc with
    Lend -> ()
  | _ -> instr i; print_cut(); all_instr i.next

let fundecl f =
  open_vbox 2;
  print_string f.fun_name; print_string ":"; print_cut();
  all_instr f.fun_body;
  close_box()
