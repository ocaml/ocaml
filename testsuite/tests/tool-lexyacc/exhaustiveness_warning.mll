(* TEST
  ocamllex_flags = " -q ";
*)
(*
   Test the exhaustiveness warning of ocamllex.
   The stderr output of ocamllex is compared against a reference.
*)

(******************************************************************)
(* Check the different kinds of hints *)
(******************************************************************)

(* missing "" *)
rule longest0 = parse
| 'a'+  { () }

(* missing _ *)
and longest1 = parse
| 'a'+  { () }
| eof   { () }

(* missing _ _ (no hint) *)
and longest2 = parse
| "aa"   { () }
| _? eof { () }

(* missing eof case *)
and shortest0 = shortest
| 'a'+  { () }
| _     { () }

(* missing '_ eof' case *)
and shortest1 = shortest
| 'a'+  { () }
| eof   { () }

(******************************************************************)
(* Check different kinds of regular expressions *)
(******************************************************************)

(* no warning *)
and empty_token = parse
| ""   { () }

(* no warning *)
and complement = parse
| ['a'-'z']   { () }
| [^'a'-'z']  { () }
| eof         { () }

(* shortest rejected input is "hello" *)
and not_hello = parse
| _? _? _? _? eof  { () }
| [^'h'] _ _ _ _   { () }
| _ [^'e'] _ _ _   { () }
| _ _ [^'l'] _ _   { () }
| _ _ _ [^'l'] _   { () }
| _ _ _ _ [^'o']   { () }

(* shortest rejected input is "hello" *)
and not_hello_shortest = shortest
| _? _? _? _? eof  { () }
| [^'h'] _ _ _ _   { () }
| _ [^'e'] _ _ _   { () }
| _ _ [^'l'] _ _   { () }
| _ _ _ [^'l'] _   { () }
| _ _ _ _ [^'o']   { () }

(* shortest rejected input is a string a length 2 *)
and string2 = parse
| _? eof           { () }

(* can't match non-empty input due to empty character class *)
and empty_char_class = parse
| _#_   { () }
| eof   { () }

(* can't match "\000a", "a\000", etc. *)
and alt = parse
| ['a'-'z'] ['a'-'z']+
| [^'a'-'z'] [^'a'-'z']+
| _? eof         { () }

(* check that the location is shown as "... line N ..." *)
and oneliner = parse eof {}
