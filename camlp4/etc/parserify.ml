(* camlp4r q_MLast.cmo *)
(* $Id$ *)

value _loc = (Token.nowhere, Token.nowhere);

type spc =
  [ SPCterm of (MLast.patt * option MLast.expr)
  | SPCnterm of MLast.patt and MLast.expr
  | SPCsterm of MLast.patt ]
;

exception NotImpl;

value rec subst v e =
  match e with
  [ <:expr< $lid:x$ >> -> if x = "strm__" then <:expr< $lid:v$ >> else e
  | <:expr< $uid:_$ >> -> e
  | <:expr< $int:_$ >> -> e
  | <:expr< $chr:_$ >> -> e
  | <:expr< $str:_$ >> -> e
  | <:expr< $e1$ . $lab$ >> -> <:expr< $subst v e1$ . $lab$ >>
  | <:expr< $x$ $y$ >> -> <:expr< $subst v x$ $subst v y$ >>
  | <:expr< let $lid:s1$ = $e1$ in $e2$ >> ->
      if s1 = v then <:expr< let $lid:s1$ = $subst v e1$ in $e2$ >>
      else <:expr< let $lid:s1$ = $subst v e1$ in $subst v e2$ >>
  | <:expr< let _ = $e1$ in $e2$ >> ->
      <:expr< let _ = $subst v e1$ in $subst v e2$ >>
  | <:expr< ($list:el$) >> -> <:expr< ($list:List.map (subst v) el$) >>
  | _ -> raise NotImpl ]
;

value rec is_free v =
  fun
  [ <:expr< $lid:x$ >> -> x <> v
  | <:expr< $uid:_$ >> -> True
  | <:expr< $int:_$ >> -> True
  | <:expr< $chr:_$ >> -> True
  | <:expr< $str:_$ >> -> True
  | <:expr< $e$ . $_$ >> -> is_free v e
  | <:expr< $x$ $y$ >> -> is_free v x && is_free v y
  | <:expr< let $lid:s1$ = $e1$ in $e2$ >> ->
      is_free v e1 && (s1 = v || is_free v e2)
  | <:expr< let _ = $e1$ in $e2$ >> -> is_free v e1 && is_free v e2
  | <:expr< ($list:el$) >> -> List.for_all (is_free v) el
  | _ -> raise NotImpl ]
;

value gensym =
  let cnt = ref 0 in
  fun () ->
    do { incr cnt; "pr_rp_symb_" ^ string_of_int cnt.val }
;

value free_var_in_expr c e =
  let rec loop_alpha v =
    let x = String.make 1 v in
    if is_free x e then Some x
    else if v = 'z' then None
    else loop_alpha (Char.chr (Char.code v + 1))
  in
  let rec loop_count cnt =
    let x = String.make 1 c ^ string_of_int cnt in
    if is_free x e then x else loop_count (succ cnt)
  in
  try
    match loop_alpha c with
    [ Some v -> v
    | None -> loop_count 1 ]
  with
  [ NotImpl -> gensym () ]
;

value parserify =
  fun
  [ <:expr< $e$ strm__ >> -> e
  | e -> <:expr< fun strm__ -> $e$ >> ]
;

value is_raise_failure =
  fun
  [ <:expr< raise Stream.Failure >> -> True
  | _ -> False ]
;

value is_raise_error =
  fun
  [ <:expr< raise (Stream.Error $_$) >> -> True
  | _ -> False ]
;

value semantic e =
  try
    if is_free "strm__" e then e
    else
      let v = free_var_in_expr 's' e in
      <:expr< let $lid:v$ = strm__ in $subst v e$ >>
  with
  [ NotImpl -> e ]
;

value rewrite_parser =
  rewrite True where rec rewrite top ge =
    match ge with
    [ <:expr< let $p$ = try $e$ with [ Stream.Failure -> raise $exc$ ] in
               $sp_kont$ >> ->
        let f = parserify e in
        <:expr<
          match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
          [ Some $p$ -> $rewrite False sp_kont$
          | _ -> raise $exc$ ]
        >>
    | <:expr< let $p$ = Stream.count strm__ in $f$ >> ->
        try
          if is_free "strm__" f then ge
          else
            let v = free_var_in_expr 's' f in
            <:expr<
              let $lid:v$ = strm__ in
              let $p$ = Stream.count strm__ in $subst v f$
            >>
        with
        [ NotImpl -> ge ]
    | <:expr< let $p$ = strm__ in $e$ >> ->
        <:expr< let $p$ = strm__ in $rewrite False e$ >>
    | <:expr< let $p$ = $f$ strm__ in $sp_kont$ >> when top ->
        <:expr<
          match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
          [ Some $p$ -> $rewrite False sp_kont$
          | _ -> raise Stream.Failure ]
        >>
    | <:expr< let $p$ = $e$ in $sp_kont$ >> ->
        if match e with
           [ <:expr< match try Some $_$ with [ Stream.Failure -> None ] with
                      [ $list:_$ ] >>
           | <:expr< match Stream.peek strm__ with [ $list:_$ ] >>
           | <:expr< try $_$ with [ Stream.Failure -> $_$ ] >>
           | <:expr< let $_$ = Stream.count strm__ in $_$ >> -> True
           | _ -> False ]
        then
          let f = rewrite True <:expr< fun strm__ -> $e$ >> in
          let exc =
            if top then <:expr< Stream.Failure >>
            else <:expr< Stream.Error "" >>
          in
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some $p$ -> $rewrite False sp_kont$
            | _ -> raise $exc$ ]
          >>
        else semantic ge
    | <:expr< match try Some $e$ with [ Stream.Failure -> None ] with
               [ Some $p$ -> $sp_kont$
               | _ -> $p_kont$ ] >> ->
        let f = parserify e in
        if not top && is_raise_failure p_kont then semantic ge
        else
          let (p, f, sp_kont, p_kont) =
            if top || is_raise_error p_kont then
              (p, f, rewrite False sp_kont, rewrite top p_kont)
            else
              let f =
                <:expr<
                  fun strm__ ->
                    match
                      try Some ($f$ strm__) with [ Stream.Failure -> None ]
                    with
                    [ Some $p$ -> $rewrite False sp_kont$
                    | _ -> $rewrite top p_kont$ ]
                >>
              in
              (<:patt< a >>, f, <:expr< a >>,
               <:expr< raise (Stream.Error "") >>)
          in
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some $p$ -> $sp_kont$
            | _ -> $p_kont$ ]
          >>
    | <:expr< match Stream.peek strm__ with [ $list:pel$ ] >> ->
        let rec iter pel =
          match pel with
          [ [(<:patt< Some $p$ >>, eo,
              <:expr< do { Stream.junk strm__; $sp_kont$ } >>);
             (<:patt< _ >>, None, p_kont) :: _] ->
               <:expr<
                 match Stream.peek strm__ with
                 [ Some $p$ $when:eo$ ->
                     do { Stream.junk strm__; $rewrite False sp_kont$ }
                 | _ -> $rewrite top p_kont$ ]
               >>
          | [(<:patt< Some $p$ >>, eo,
              <:expr< do { Stream.junk strm__; $sp_kont$ } >>) :: pel] ->
               let p_kont = iter pel in
               <:expr<
                 match Stream.peek strm__ with
                 [ Some $p$ $when:eo$ ->
                     do { Stream.junk strm__; $rewrite False sp_kont$ }
                 | _ -> $p_kont$ ]
               >>
          | _ ->
              <:expr< match Stream.peek strm__ with [ $list:pel$ ] >> ]
        in
        iter pel
    | <:expr< try Some $e$ with [ Stream.Failure -> $p_kont$ ] >> ->
        let f = parserify e in
        let e =
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> Some a
            | _ -> $p_kont$ ]
          >>
        in
        rewrite top e
    | <:expr< try $e$ with [ Stream.Failure -> $p_kont$ ] >> ->
        let f = parserify e in
        let e =
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> a
            | _ -> $rewrite top p_kont$ ]
          >>
        in
        rewrite top e
    | <:expr< $f$ strm__ >> ->
        if top then
          <:expr<
            match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
            [ Some a -> a
            | _  -> raise Stream.Failure ]
          >>
        else
          let v = free_var_in_expr 's' f in
          <:expr< let $lid:v$ = strm__ in $subst v f$ $lid:v$ >>
    | e -> semantic e ]
;

value spc_of_parser =
  let rec parser_cases e =
    match e with
    [ <:expr<
        match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
        [ Some $p$ -> $sp_kont$
        | _ -> $p_kont$ ]
      >> ->
        let spc = (SPCnterm p f, None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e) :: parser_cases p_kont]
    | <:expr<
        match Stream.peek strm__ with
        [ Some $p$ $when:wo$ -> do { Stream.junk strm__; $sp_kont$ }
        | _ -> $p_kont$ ]
      >> ->
        let spc = (SPCterm (p, wo), None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e) :: parser_cases p_kont]
    | <:expr< let $p$ = strm__ in $sp_kont$ >> ->
        let spc = (SPCsterm p, None) in
        let (sp, epo, e) = kont sp_kont in
        [([spc :: sp], epo, e)]
    | <:expr< let $p$ = Stream.count strm__ in $e$ >> -> [([], Some p, e)]
    | <:expr< raise Stream.Failure >> -> []
    | _ -> [([], None, e)] ]
  and kont e =
    match e with
    [ <:expr<
        match try Some ($f$ strm__) with [ Stream.Failure -> None ] with
        [ Some $p$ -> $sp_kont$
        | _ -> raise (Stream.Error $err$) ]
      >> ->
        let err =
          match err with
          [ <:expr< "" >> -> None
          | _ -> Some err ]
        in
        let spc = (SPCnterm p f, err) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr<
        match Stream.peek strm__ with
        [ Some $p$ $when:wo$ -> do { Stream.junk strm__; $sp_kont$ }
        | _ -> raise (Stream.Error $err$) ]
      >> ->
        let err =
          match err with
          [ <:expr< "" >> -> None
          | _ -> Some err ]
        in
        let spc = (SPCterm (p, wo), err) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr< let $p$ = strm__ in $sp_kont$ >> ->
        let spc = (SPCsterm p, None) in
        let (sp, epo, e) = kont sp_kont in
        ([spc :: sp], epo, e)
    | <:expr< let $p$ = Stream.count strm__ in $e$ >> -> ([], Some p, e)
    | _ -> ([], None, e) ]
  in
  parser_cases
;

value parser_of_expr e = spc_of_parser (rewrite_parser e);
