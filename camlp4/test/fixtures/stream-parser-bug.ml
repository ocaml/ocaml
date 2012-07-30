let foo = parser
  | [< '42; ps >] ->
      let ps = ps + 42 in
      type_phrases ps
  | [< >] -> [< >]
