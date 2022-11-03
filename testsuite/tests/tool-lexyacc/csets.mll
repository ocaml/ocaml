(* TEST
  ocamllex_flags = " -q "
*)

let digit = ['0'-'9']
let alpha = ['a'-'z']
let alpha' = (digit | alpha) # digit

rule read = parse
| alpha'+ as lxm { Some lxm }
| digit+ as lxm { Some lxm }
| eof { None }

{
let () =
  let rec aux lexbuf =
    match read lexbuf with
    | Some x -> x :: aux lexbuf
    | None -> []
  in
  List.iter print_endline (aux (Lexing.from_string "abc0345ghz"))
}
