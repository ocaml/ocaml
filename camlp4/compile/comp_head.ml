(* camlp4r q_MLast.cmo pa_extend.cmo *)
(* $Id$ *)

module P =
  struct
    value gloc bp strm = Grammar.loc_of_token_interval bp (Stream.count strm);
    value list0 symb =
      let rec loop al =
        parser
        [ [: a = symb; s :] -> loop [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = loop [] :] -> List.rev a
    ;
    value list0sep symb sep =
      let rec kont al =
        parser
        [ [: v = sep; a = symb; s :] -> kont [a :: al] s
        | [: :] -> al ]
      in
      parser
      [ [: a = symb; s :] -> List.rev (kont [a] s)
      | [: :] -> [] ]
    ;
    value list1 symb =
      let rec loop al =
        parser
        [ [: a = symb; s :] -> loop [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = symb; s :] -> List.rev (loop [a] s)
    ;
    value list1sep symb sep =
      let rec kont al =
        parser
        [ [: v = sep; a = symb; s :] -> kont [a :: al] s
        | [: :] -> al ]
      in
      parser [: a = symb; s :] -> List.rev (kont [a] s)
    ;
    value option f =
      parser
      [ [: x = f :] -> Some x
      | [: :] -> None ]
    ;
    value token (p_con, p_prm) =
      if p_prm = "" then parser [: `(con, prm) when con = p_con :] -> prm
      else parser [: `(con, prm) when con = p_con && prm = p_prm :] -> prm
    ;
    value orzero f f0 =
      parser bp
      [ [: x = f :] -> x
      | [: x = f0 :] ep ->
(*
let (loc1, loc2) = Grammar.loc_of_token_interval bp ep in
let _ = do { Printf.eprintf "recovered or_zero at loc (%d, %d)\n" loc1 loc2; flush stderr } in
*)
           x ]
    ;
    value error entry prev_symb symb =
      symb ^ " expected" ^
      (if prev_symb = "" then "" else " after " ^ prev_symb) ^
      " (in [" ^ entry ^ "])"
    ;
    value ((lexer,pos) as lexer_pos) = Plexer.make_lexer();
  end
;

(****************************************)

