(* Transformation of Mach code into a list of pseudo-instructions. *)

type label = int
val new_label: unit -> label

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    live: Reg.Set.t }

and instruction_desc =
    Lend
  | Lop of Mach.operation
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of Mach.test * label
  | Lswitch of label array
  | Lpushtrap of label
  | Lpoptrap
  | Lentertrap
  | Lraise

type fundecl =
  { fun_name: string;
    fun_body: instruction }

val fundecl: Mach.fundecl -> fundecl

