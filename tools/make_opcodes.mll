(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Nicolas Ojeda Bar, LexiFi                    *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let opcode = ['A'-'Z''0'-'9''_']+
let space = [' ''\n''\t']*

rule find_enum = parse
| "enum" [^'{']* '{'        { opnames lexbuf }
| _                         { find_enum lexbuf}

and opnames = parse
| space (opcode as op)  ',' { op :: opnames lexbuf }
| space opcode space '}'    { [] }

{
  let print_names = ref false
  let print_opcodes = ref false

  open Printf

  let () =
    let spec =
      [
        "-names", Arg.Set print_names, "Print names";
        "-opcodes", Arg.Set print_opcodes, "Print opcodes";
      ]
    in
    Arg.parse spec ignore "make_opcodes";
    let lexbuf = Lexing.from_channel stdin in
    let opnames = find_enum lexbuf in
    if !print_names then begin
      printf "let names_of_instructions = [|\n";
      List.iter (fun s -> printf "  %S;\n" s) opnames;
      printf "\n|]\n"
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
