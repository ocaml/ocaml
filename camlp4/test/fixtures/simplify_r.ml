open Camlp4.PreCast;

let simplify =
  object (self)
    inherit Ast.map as super;
    method expr e =
      match e with
      [ <:expr< $x$ + 0 >> | <:expr< 0 + $x$ >> -> self#expr x
      | e -> super#expr e ];
  end
in AstFilters.register_str_item_filter simplify#str_item;
