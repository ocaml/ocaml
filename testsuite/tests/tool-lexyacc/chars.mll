(* TEST
  ocamllex_flags = " -q "
*)

{
let f' _ = ()
let f1 _ = ()
let f2 _ _ = ()
}

rule token = parse
  | 'a' { f' '"' }
  | 'b' { f2 '\o170' '"' }
  | 'c' { f1 "\u{1F42B}" }
  | 'd' { f1 {|}|} }
  | 'e' { (* " *) } (* " *) }
  | 'f' { (* {%foo bar| *) } (* |bar} *) }
