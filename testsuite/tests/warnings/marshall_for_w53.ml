let w53_ast =
  (Pparse.parse_implementation ~tool_name:"w53_test" "w53.ml").Pparse.ast

let () = Pparse.write_ast Pparse.Structure "w53.marshalled.ml" w53_ast
