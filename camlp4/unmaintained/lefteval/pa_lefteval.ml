(* pa_r.cmo q_MLast.cmo pr_dump.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                               Camlp4                                *)
(*                                                                     *)
(*    Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file                 *)
(*   ../../../LICENSE.                                                 *)
(*                                                                     *)
(***********************************************************************)
(* $Id$ *)

value not_impl name x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  failwith ("pa_lefteval: not impl: " ^ name ^ "; " ^ desc ^ ">")
;

value rec expr_fa al =
  fun
  [ <:expr< $f$ $a$ >> -> expr_fa [a :: al] f
  | f -> (f, al) ]
;

(* generating let..in before functions calls which evaluates
   several (more than one) of their arguments *)

value no_side_effects_ht =
  let ht = Hashtbl.create 73 in
  do {
    List.iter (fun s -> Hashtbl.add ht s True)
      ["<"; "="; "@"; "^"; "+"; "-"; "ref"];
    ht
  }
;

value no_side_effects =
  fun
  [ <:expr< $uid:_$ >> -> True
  | <:expr< $uid:_$ . $uid:_$ >> -> True
  | <:expr< $lid:s$ >> ->
      try Hashtbl.find no_side_effects_ht s with [ Not_found -> False ]
  | _ -> False ]
;

value rec may_side_effect =
  fun
  [ <:expr< $lid:_$ >> | <:expr< $uid:_$ >> | <:expr< $str:_$ >> |
    <:expr< $chr:_$ >> | <:expr< $int:_$ >> | <:expr< $flo:_$ >> |
    <:expr< $_$ . $_$ >> | <:expr< fun [ $list:_$ ] >> ->
      False
  | <:expr< ($list:el$) >> -> List.exists may_side_effect el
  | <:expr< $_$ $_$ >> as e ->
      let (f, el) = expr_fa [] e in
      not (no_side_effects f) || List.exists may_side_effect el
  | _ -> True ]
;

value rec may_be_side_effect_victim =
  fun
  [ <:expr< $lid:_$ . $_$ >> -> True
  | <:expr< $uid:_$ . $e$ >> -> may_be_side_effect_victim e
  | _ -> False ]
;

value rec may_depend_on_order el =
  loop False False el where rec loop
    side_effect_found side_effect_victim_found =
    fun
    [ [e :: el] ->
        if may_side_effect e then
          if side_effect_found || side_effect_victim_found then True
          else loop True True el
        else if may_be_side_effect_victim e then
          if side_effect_found then True else loop False True el
        else loop side_effect_found side_effect_victim_found el
    | [] -> False ]
;

value gen_let_in loc expr el =
  let (pel, el) =
    loop 0 (List.rev el) where rec loop n =
      fun
      [ [e :: el] ->
          if may_side_effect e || may_be_side_effect_victim e then
            if n = 0 then
              let (pel, el) = loop 1 el in
              (pel, [expr e :: el])
            else
              let id = "xxx" ^ string_of_int n in
              let (pel, el) = loop (n + 1) el in
              ([(<:patt< $lid:id$ >>, expr e) :: pel],
               [<:expr< $lid:id$ >> :: el])
          else
            let (pel, el) = loop n el in
            (pel, [expr e :: el])
      | [] -> ([], []) ]
  in
  match List.rev el with
  [ [e :: el] -> (pel, e, el)
  | _ -> assert False ]
;

value left_eval_apply loc expr e1 e2 =
  let (f, el) = expr_fa [] <:expr< $e1$ $e2$ >> in
  if not (may_depend_on_order [f :: el]) then <:expr< $expr e1$ $expr e2$ >>
  else
    let (pel, e, el) = gen_let_in loc expr [f :: el] in
    let e = List.fold_left (fun e e1 -> <:expr< $e$ $e1$ >>) e el in
    List.fold_left (fun e (p1, e1) -> <:expr< let $p1$ = $e1$ in $e$ >>) e pel
;

value left_eval_tuple loc expr el =
  if not (may_depend_on_order el) then <:expr< ($list:List.map expr el$) >>
  else
    let (pel, e, el) = gen_let_in loc expr el in
    List.fold_left (fun e (p1, e1) -> <:expr< let $p1$ = $e1$ in $e$ >>)
      <:expr< ($list:[e :: el]$) >> pel
;

value left_eval_record loc expr lel =
  let el = List.map snd lel in
  if not (may_depend_on_order el) then
    let lel = List.map (fun (p, e) -> (p, expr e)) lel in
    <:expr< { $list:lel$ } >>
  else
    let (pel, e, el) = gen_let_in loc expr el in
    let e =
      let lel = List.combine (List.map fst lel) [e :: el] in
      <:expr< { $list:lel$ } >>
    in
    List.fold_left (fun e (p1, e1) -> <:expr< let $p1$ = $e1$ in $e$ >>) e pel
;

value left_eval_assign loc expr e1 e2 = <:expr< $e1$ := $expr e2$ >>;

(* scanning the input tree, calling "left_eval_*" functions if necessary *)

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value class_infos f ci =
  {MLast.ciLoc = ci.MLast.ciLoc; MLast.ciVir = ci.MLast.ciVir;
   MLast.ciPrm = ci.MLast.ciPrm; MLast.ciNam = ci.MLast.ciNam;
   MLast.ciExp = f ci.MLast.ciExp}
;

value rec expr x =
  let loc = MLast.loc_of_expr x in
  match x with
  [ <:expr< fun [ $list:pwel$ ] >> ->
      <:expr< fun [ $list:List.map match_assoc pwel$ ] >>
  | <:expr< match $e$ with [ $list:pwel$ ] >> ->
      <:expr< match $expr e$ with [ $list:List.map match_assoc pwel$ ] >>
  | <:expr< try $e$ with [ $list:pwel$ ] >> ->
      <:expr< try $expr e$ with [ $list:List.map match_assoc pwel$ ] >>
  | <:expr< let $opt:rf$ $list:pel$ in $e$ >> ->
      <:expr< let $opt:rf$ $list:List.map let_binding pel$ in $expr e$ >>
  | <:expr< let module $s$ = $me$ in $e$ >> ->
      <:expr< let module $s$ = $module_expr me$ in $expr e$ >>
  | <:expr< if $e1$ then $e2$ else $e3$ >> ->
      <:expr< if $expr e1$ then $expr e2$ else $expr e3$ >>
  | <:expr< while $e$ do { $list:el$ } >> ->
      <:expr< while $expr e$ do { $list:List.map expr el$ } >>
  | <:expr< do { $list:el$ } >> -> <:expr< do { $list:List.map expr el$ } >>
  | <:expr< $e$ # $s$ >> -> <:expr< $expr e$ # $s$ >>
  | <:expr< ($e$ : $t$) >> -> <:expr< ($expr e$ : $t$) >>
  | <:expr< $e1$ || $e2$ >> -> <:expr< $expr e1$ || $expr e2$ >>
  | <:expr< $e1$ && $e2$ >> -> <:expr< $expr e1$ && $expr e2$ >>
  | <:expr< $e1$ $e2$ >> -> left_eval_apply loc expr e1 e2
  | <:expr< ($list:el$) >> -> left_eval_tuple loc expr el
  | <:expr< { $list:lel$ } >> -> left_eval_record loc expr lel
  | <:expr< $e1$ := $e2$ >> -> left_eval_assign loc expr e1 e2
  | <:expr< $_$ . $_$ >> | <:expr< $uid:_$ >> | <:expr< $lid:_$ >> |
    <:expr< $str:_$ >> | <:expr< $chr:_$ >> | <:expr< $int:_$ >> |
    <:expr< $flo:_$ >> | <:expr< new $list:_$ >> ->
      x
  | x -> not_impl "expr" x ]
and let_binding (p, e) = (p, expr e)
and match_assoc (p, eo, e) = (p, map_option expr eo, expr e)
and module_expr x =
  let loc = MLast.loc_of_module_expr x in
  match x with
  [ <:module_expr< functor ($s$ : $mt$) -> $me$ >> ->
      <:module_expr< functor ($s$ : $mt$) -> $module_expr me$ >>
  | <:module_expr< ($me$ : $mt$) >> ->
      <:module_expr< ($module_expr me$ : $mt$) >>
  | <:module_expr< struct $list:sil$ end >> ->
      <:module_expr< struct $list:List.map str_item sil$ end >>
  | <:module_expr< $_$ . $_$ >> | <:module_expr< $_$ $_$ >> |
    <:module_expr< $uid:_$ >> ->
      x ]
and str_item x =
  let loc = MLast.loc_of_str_item x in
  match x with
  [ <:str_item< module $s$ = $me$ >> ->
      <:str_item< module $s$ = $module_expr me$ >>
  | <:str_item< value $opt:rf$ $list:pel$ >> ->
      <:str_item< value $opt:rf$ $list:List.map let_binding pel$ >>
  | <:str_item< declare $list:sil$ end >> ->
      <:str_item< declare $list:List.map str_item sil$ end >>
  | <:str_item< class $list:ce$ >> ->
      <:str_item< class $list:List.map (class_infos class_expr) ce$ >>
  | <:str_item< $exp:e$ >> -> <:str_item< $exp:expr e$ >>
  | <:str_item< open $_$ >> | <:str_item< type $list:_$ >> |
    <:str_item< exception $_$ of $list:_$ = $_$ >> |
    <:str_item< module type $_$ = $_$ >> | <:str_item< # $_$ $opt:_$ >> ->
      x
  | x -> not_impl "str_item" x ]
and class_expr x =
  let loc = MLast.loc_of_class_expr x in
  match x with
  [ <:class_expr< object $opt:p$ $list:csil$ end >> ->
      <:class_expr< object $opt:p$ $list:List.map class_str_item csil$ end >>
  | x -> not_impl "class_expr" x ]
and class_str_item x =
  let loc = MLast.loc_of_class_str_item x in
  match x with
  [ <:class_str_item< value $opt:mf$ $s$ = $e$ >> ->
      <:class_str_item< value $opt:mf$ $s$ = $expr e$ >>
  | <:class_str_item< method $s$ = $e$ >> ->
      <:class_str_item< method $s$ = $expr e$ >>
  | x -> not_impl "class_str_item" x ]
;

value parse_implem = Pcaml.parse_implem.val;
value parse_implem_with_left_eval strm =
  let (r, b) = parse_implem strm in
  (List.map (fun (si, loc) -> (str_item si, loc)) r, b)
;
Pcaml.parse_implem.val := parse_implem_with_left_eval;
