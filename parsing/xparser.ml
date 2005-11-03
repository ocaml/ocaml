(* Variable shared by the lexer and the parser. *)
(* Used to be in Location, but this changes the MD5 of its interface.
   Issues with dynamic linking. *)
let ext = ref false
