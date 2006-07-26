open Camlp4.PreCast;

value ghost = Loc.ghost;

value global_handler_ref = ref <:expr@ghost<>>;

value find_global_handler = object
  inherit Ast.map as super;
  method str_item st = do {
    match st with
    [ <:str_item< value global_handler = $f$ >> -> global_handler_ref.val := f
    | _ -> () ];
    super#str_item st
  };
end;

AstFilters.register_str_item_filter
  (fun st ->
    let _ = find_global_handler#str_item st in
    <:str_item@ghost< try let module Main = struct $st$ end in ()
                      with e -> $global_handler_ref.val$ e >>);

