(* Generation of bytecode for .cmo files *)

open Lambda
open Instruct

(* Relocation information *)

type reloc_info =
    Reloc_literal of structured_constant    (* structured constant *)
  | Reloc_getglobal of Ident.t             (* reference to a global *)
  | Reloc_setglobal of Ident.t             (* definition of a global *)
  | Reloc_primitive of string               (* C primitive number *)

(* Descriptor for compilation units *)

type compilation_unit =
  { mutable cu_pos: int;                (* Absolute position in file *)
    cu_codesize: int;                   (* Size of code block *)
    cu_reloc: (reloc_info * int) list;  (* Relocation information *)
    cu_interfaces: (string * int) list } (* Names and CRC of intfs imported *)

(* Format of a .cmo file:
     Obj.magic number (Config.cmo_magic_number)
     absolute offset of compilation unit descriptor
     block of relocatable bytecode
     compilation unit descriptor *)

val to_file: out_channel -> string -> int -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             CRC of interface implemented
             list of instructions to emit *)
val to_memory: instruction list -> instruction list ->
                    string * int * (reloc_info * int) list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             size of this block
             relocation information *)

