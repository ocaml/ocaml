(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                      Nicolas Ojeda Bar, LexiFi                      *)
(*                                                                     *)
(*  Copyright 2016 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let ident = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let space = [' ''\n''\t']*

rule find_enum = parse
| "enum" space (ident as id) space '{' { id, opnames lexbuf }
| _                                    { find_enum lexbuf }

and opnames = parse
| space (ident as op) space ','        { op :: opnames lexbuf }
| space ident space '}'                { [] }

{
  let print_opnames = ref false
  let print_opcodes = ref false

  open Printf

  let () =
    let spec =
      [
        "-opnames", Arg.Set print_opnames, " Dump opcode names";
        "-opcodes", Arg.Set print_opcodes, " Dump opcode numbers";
      ]
    in
    Arg.parse (Arg.align spec) ignore "Extract opcode info from instruct.h";
    let lexbuf = Lexing.from_channel stdin in
    let id, opnames = find_enum lexbuf in
    if !print_opnames then begin
      printf "let names_of_%s = [|\n" id;
      List.iter (fun s -> printf "  %S;\n" s) opnames;
      printf "|]\n"
    end;
    if !print_opcodes then begin
      let rec loop i = function
        | op :: rest ->
            printf "let op%s = %i\n" op i;
            loop (i+1) rest
        | [] -> ()
      in
      loop 0 opnames
    end
}
