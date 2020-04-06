(* TEST
  ocamllex_flags = " -q "
*)

rule read = shortest
      | ("aa" | "bbb") (_ as x) _? { x }
      | _ as y { y }

{
 let r = read (Lexing.from_string "aasdf") in
 Printf.printf "<%c>\n" r ;
 ()
}
