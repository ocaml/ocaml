open Camlp4;                                             (* -*- camlp4r -*- *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nao Hirokawa: initial version
 * - Nicolas Pouillard: revised syntax version
 *)


module Id = struct
  value name = "Camlp4ListComprenhsion";
  value version = Sys.ocaml_version;
end;

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  value rec loop n =
    fun
    [ [] -> None
    | [(x, _)] -> if n = 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ];

  value stream_peek_nth n strm = loop n (Stream.npeek n strm);

  (* usual trick *)
  value test_patt_lessminus =
    Gram.Entry.of_parser "test_patt_lessminus"
      (fun strm ->
        let rec skip_patt n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD "<-") -> n
          | Some (KEYWORD ("[" | "[<")) ->
              skip_patt (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") -> 
              skip_patt (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") -> 
              skip_patt (ignore_upto "}" (n + 1) + 1)
          | Some (KEYWORD ("as" | "::" | "," | "_"))
          | Some (LIDENT _ | UIDENT _) -> skip_patt (n + 1)
          | Some _ | None -> raise Stream.Failure ]
        and ignore_upto end_kwd n =
          match stream_peek_nth n strm with
          [ Some (KEYWORD prm) when prm = end_kwd -> n 
          | Some (KEYWORD ("[" | "[<")) ->
              ignore_upto end_kwd (ignore_upto "]" (n + 1) + 1)
          | Some (KEYWORD "(") ->
              ignore_upto end_kwd (ignore_upto ")" (n + 1) + 1)
          | Some (KEYWORD "{") -> 
              ignore_upto end_kwd (ignore_upto "}" (n + 1) + 1)
          | Some _ -> ignore_upto end_kwd (n + 1)
          | None -> raise Stream.Failure ]
        in
        skip_patt 1);

  value map _loc p e l =
    match (p, e) with
    [ (<:patt< $lid:x$ >>, <:expr< $lid:y$ >>) when x = y -> l
    | _ -> 
        if Ast.is_irrefut_patt p then
          <:expr< List.map (fun $p$ -> $e$) $l$ >>
        else
          <:expr< List.fold_right
                    (fun
                      [ $pat:p$ when True -> (fun x xs -> [ x :: xs ]) $e$
                      | _ -> (fun l -> l) ])
                    $l$ [] >> ];

  value filter _loc p b l =
    if Ast.is_irrefut_patt p then
      <:expr< List.filter (fun $p$ -> $b$) $l$ >>
    else
      <:expr< List.filter (fun [ $p$ when True -> $b$ | _ -> False ]) $l$ >>;

  value concat _loc l = <:expr< List.concat $l$ >>;

  value rec compr _loc e =
    fun
    [ [`gen (p, l)] -> map _loc p e l
    | [`gen (p, l); `cond b :: items] ->
        compr _loc e [`gen (p, filter _loc p b l) :: items]
    | [`gen (p, l) :: ([ `gen (_, _) :: _ ] as is )] -> 
        concat _loc (map _loc p (compr _loc e is) l) 
    | _ -> raise Stream.Failure ];

  DELETE_RULE Gram expr: "["; sem_expr_for_list; "]" END;

  value is_revised =
    try do {
      DELETE_RULE Gram expr: "["; sem_expr_for_list; "::"; expr; "]" END;
      True
    } with [ Not_found -> False ];

  value comprehension_or_sem_expr_for_list =
    Gram.Entry.mk "comprehension_or_sem_expr_for_list";

  EXTEND Gram
    GLOBAL: expr comprehension_or_sem_expr_for_list;

    expr: LEVEL "simple"
      [ [ "["; e = comprehension_or_sem_expr_for_list; "]" -> e ] ]
    ;

    comprehension_or_sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; mk = sem_expr_for_list ->
            <:expr< [ $e$ :: $mk <:expr< [] >>$ ] >>
        | e = expr LEVEL "top"; ";" -> <:expr< [$e$] >>
        | e = expr LEVEL "top"; "|"; l = LIST1 item SEP ";" -> compr _loc e l
        | e = expr LEVEL "top" -> <:expr< [$e$] >> ] ]
    ;

    item: 
      [ [ test_patt_lessminus; 
          p = patt; "<-" ; e = expr LEVEL "top" -> `gen (p, e)
        | e = expr LEVEL "top" -> `cond e ] ]
    ;

  END;

  if is_revised then
    EXTEND Gram
      GLOBAL: expr comprehension_or_sem_expr_for_list;

      comprehension_or_sem_expr_for_list:
      [ [ e = expr LEVEL "top"; ";"; mk = sem_expr_for_list; "::"; last = expr ->
            <:expr< [ $e$ :: $mk last$ ] >>
        | e = expr LEVEL "top"; "::"; last = expr ->
            <:expr< [ $e$ :: $last$ ] >> ] ]
      ;
    END
  else ();

end;

let module M = Register.OCamlSyntaxExtension Id Make in ();
