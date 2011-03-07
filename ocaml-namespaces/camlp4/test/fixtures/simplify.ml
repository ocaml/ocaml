open Camlp4.PreCast

let simplify =
  object
    inherit Ast.map as super
    method expr e =
      match super#expr e with
      | <:expr< $x$ + 0 >> | <:expr< 0 + $x$ >> -> x
      | x -> x
  end
in AstFilters.register_str_item_filter simplify#str_item
