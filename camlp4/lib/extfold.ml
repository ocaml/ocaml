(* camlp4r *)
(* $Id$ *)

type t 'te 'a 'b =
  Gramext.g_entry 'te -> list (Gramext.g_symbol 'te) ->
    (Stream.t 'te -> 'a) -> Stream.t 'te -> 'b
;

type tsep 'te 'a 'b =
  Gramext.g_entry 'te -> list (Gramext.g_symbol 'te) ->
    (Stream.t 'te -> 'a) -> (Stream.t 'te -> unit) -> Stream.t 'te -> 'b
;

value gen_fold0 final f e entry symbl psymb =
  let rec fold accu =
    parser
    [ [: a = psymb; s :] -> fold (f a accu) s
    | [: :] -> accu ]
  in
  parser [: a = fold e :] -> final a
;

value gen_fold1 final f e entry symbl psymb =
  let rec fold accu =
    parser
    [ [: a = psymb; s :] -> fold (f a accu) s
    | [: :] -> accu ]
  in
  parser [: a = psymb; a = fold (f a e) :] -> final a
;

value gen_fold0sep final f e entry symbl psymb psep =
  let failed =
    fun
    [ [symb; sep] -> Grammar.symb_failed_txt entry sep symb
    | _ -> "failed" ]
  in
  let rec kont accu =
    parser
    [ [: v = psep; a = psymb ? failed symbl; s :] -> kont (f a accu) s
    | [: :] -> accu ]
  in
  parser
  [ [: a = psymb; s :] -> final (kont (f a e) s)
  | [: :] -> e ]
;

value gen_fold1sep final f e entry symbl psymb psep =
  let failed =
    fun
    [ [symb; sep] -> Grammar.symb_failed_txt entry sep symb
    | _ -> "failed" ]
  in
  let parse_top =
    fun
    [ [symb; _] -> Grammar.parse_top_symb entry symb
    | _ -> raise Stream.Failure ]
  in
  let rec kont accu =
    parser
    [ [: v = psep;
         a =
           parser
           [ [: a = psymb :] -> a
           | [: a = parse_top symbl :] -> Obj.magic a
           | [: :] -> raise (Stream.Error (failed symbl)) ];
         s :] ->
        kont (f a accu) s
    | [: :] -> accu ]
  in
  parser [: a = psymb; s :] -> final (kont (f a e) s)
;

value sfold0 f e = gen_fold0 (fun x -> x) f e;
value sfold1 f e = gen_fold1 (fun x -> x) f e;
value sfold0sep f e = gen_fold0sep (fun x -> x) f e;
value sfold1sep f e = gen_fold1sep (fun x -> x) f e;

value cons x y = [x :: y];
value nil = [];

value slist0 entry = gen_fold0 List.rev cons nil entry;
value slist1 entry = gen_fold1 List.rev cons nil entry;
value slist0sep entry = gen_fold0sep List.rev cons nil entry;
value slist1sep entry = gen_fold1sep List.rev cons nil entry;

value sopt entry symbl psymb =
  parser
  [ [: a = psymb :] -> Some a
  | [: :] -> None ]
;
