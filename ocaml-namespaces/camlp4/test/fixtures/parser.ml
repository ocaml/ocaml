open Camlp4.PreCast;
type t = [ A of t and t | B of string ];
value lex = Lexer.mk ();

    (* value list0 symb =
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
    ;                                                       *)

value list1 =
  let rec self stream acc =
    match stream with parser
    [ [: `(EOI, _) :] -> acc
    | [: `(LIDENT x, _); xs :] -> self xs (A acc (B x))
    | [: `(BLANKS _ | NEWLINE, _); xs :] -> self xs acc ]
  in
  parser [: `(LIDENT x, _); xs :] -> self xs (B x);
value rec length x acc =
  match x with
  [ A x y -> length x (length y acc)
  | B _ -> succ acc ];
(* value length _ _ = -1; *)
open Format;
try
  let f = Sys.argv.(1) in
  let () = printf "parsing...@." in
  let a = list1 (lex (Loc.mk f) (Stream.of_channel (open_in f))) in
  let () = printf "counting...@." in
  let n = length a 0 in
  printf "%d@." n
with e -> eprintf "error: %a@." Camlp4.ErrorHandler.print e;
