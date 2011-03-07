open Camlp4.PreCast;;
let foldr_funs = ref [];;
let foldl_funs = ref [];;
AstFilters.register_str_item_filter begin
  Ast.map_expr begin function
  | <:expr@loc< def_foldr $lid:name$ $e$ >> ->
      foldr_funs := (name, e) :: !foldr_funs; <:expr@loc<()>>
  | <:expr@loc< def_foldl $lid:name$ $e$ >> ->
      foldl_funs := (name, e) :: !foldl_funs; <:expr@loc<()>>
  | e -> e
  end
end#str_item;;
AstFilters.register_str_item_filter begin
  Ast.map_expr begin function
  | <:expr@loc< $lid:name$($tup:e$) >> when List.mem_assoc name !foldl_funs ->
      let op = List.assoc name !foldl_funs in
      let rec foldl =
        function
        | [] -> assert false
        | [x] -> x
        | x :: xs -> <:expr@loc< $op$ $foldl xs$ $x$ >>
      in foldl (List.rev (Ast.list_of_expr e []))
  | <:expr@loc< $lid:name$($tup:e$) >> when List.mem_assoc name !foldr_funs ->
      let op = List.assoc name !foldr_funs in
      let rec foldr =
        function
        | [] -> assert false
        | [x] -> x
        | x :: xs -> <:expr@loc< $op$ $x$ $foldr xs$ >>
      in foldr (Ast.list_of_expr e [])
  | e -> e
  end
end#str_item;;
(*

AstFilters.register_str_item_filter begin
  Ast.map_expr begin function
  | <:expr@loc< foldl($lid:op$, $e$) >> ->
      let rec foldl =
        function
        | [] -> assert false
        | [x] -> x
        | x :: xs -> <:expr@loc< $lid:op$ $foldl xs$ $x$ >>
      in foldl (List.rev (Ast.list_of_expr e []))
  | <:expr@loc< foldr($lid:op$, $e$) >> ->
      let rec foldr =
        function
        | [] -> assert false
        | [x] -> x
        | x :: xs -> <:expr@loc< $lid:op$ $x$ $foldr xs$ >>
      in foldr (Ast.list_of_expr e [])
  | e -> e
  end
end#str_item;;

AstFilters.register_str_item_filter begin
  Ast.map_expr begin function
  | <:expr@loc< \!+ ($tup:e$) >> ->
      begin match Ast.list_of_expr e [] with
      | [] | [_] -> assert false
      | x :: xs -> List.fold_left (fun acc x -> <:expr@loc< $acc$ + $x$ >>) x xs
      end
  | <:expr< \!+ $e$ >> -> e
  | e -> e
  end
end#str_item;;
*)
