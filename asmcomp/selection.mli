(* Instruction selection and choice of evaluation order. *)

type expression =
    Sconst of Cmm.constant
  | Svar of Ident.t
  | Slet of Ident.t * expression * expression
  | Sassign of Ident.t * expression
  | Stuple of expression array * int list
  | Sop of Mach.operation * expression * Cmm.machtype
  | Sproj of expression * int * int
  | Ssequence of expression * expression
  | Sifthenelse of Mach.test * expression * expression * expression
  | Sswitch of expression * int array * expression array
  | Sloop of expression * Mach.test * expression
  | Scatch of expression * expression
  | Sexit
  | Strywith of expression * Ident.t * expression
  | Sraise of expression

val expression: Cmm.expression -> expression
