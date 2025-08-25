(* usage: parse_and_marshall.exe [input] [output]

   This program parses [input] (which is expected to be an .ml file) and write a
   marshalled ast to [output]. *)
let () =
  if Array.length Sys.argv != 3 then failwith "Expected two arguments";
  let input_name = Sys.argv.(1) in
  let output_name = Sys.argv.(2) in
  let ast = Pparse.parse_implementation ~tool_name:"parse_and_marshall" input_name in
  Pparse.write_ast Pparse.Structure output_name ast
