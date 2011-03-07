open Camlp4.PreCast;;

let data_constructor_arguments _loc n t =
  let rec self n =
    if n <= 0 then <:ctyp<>> else <:ctyp< $t$ and $self (n-1)$ >>
  in self n
;;

let data_constructor _loc n t =
  <:ctyp< $uid:"C"^string_of_int n$ of $data_constructor_arguments _loc n t$ >>
;;

let gen_type _loc n t =
  let rec self n =
    if n <= 0 then <:ctyp<>>
    else <:ctyp< $self (n-1)$ | $data_constructor _loc n t$ >>
  in <:ctyp< [ $self n$ ] >>
;;

let filter =
  function
  | <:ctyp@_loc< gen_type $lid:x$ >> | <:ctyp@_loc< $lid:x$ gen_type >> ->
      Scanf.sscanf x "%[^0-9]%d" begin fun _ n ->
        gen_type _loc n <:ctyp< $lid:x$ >>
      end
  | t -> t
;;

AstFilters.register_str_item_filter (Ast.map_ctyp filter)#str_item;;

IFDEF TEST THEN
  type t7 = gen_type t7;;
ENDIF;;
