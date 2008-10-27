(* camlp4r *)
(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)



(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)
module Make (Structure : Structure.S) = struct
  open Structure;
  open Format;
  module Parse = Parser.Make Structure;
  module Fail = Failed.Make Structure;
  open Sig.Grammar;

  module Stream = struct
    include Stream;
    value junk strm = Context.junk strm;
    value count strm = Context.bp strm;
  end;

  value sfold0 f e _entry _symbl psymb =
    let rec fold accu =
      parser
      [ [: a = psymb; s :] -> fold (f a accu) s
      | [: :] -> accu ]
    in
    parser [: a = fold e :] -> a
  ;

  value sfold1 f e _entry _symbl psymb =
    let rec fold accu =
      parser
      [ [: a = psymb; s :] -> fold (f a accu) s
      | [: :] -> accu ]
    in
    parser [: a = psymb; a = fold (f a e) :] -> a
  ;

  value sfold0sep f e entry symbl psymb psep =
    let failed =
      fun
      [ [symb; sep] -> Fail.symb_failed_txt entry sep symb
      | _ -> "failed" ]
    in
    let rec kont accu =
      parser
      [ [: () = psep; a = psymb ?? failed symbl; s :] -> kont (f a accu) s
      | [: :] -> accu ]
    in
    parser
    [ [: a = psymb; s :] -> kont (f a e) s
    | [: :] -> e ]
  ;

  value sfold1sep f e entry symbl psymb psep =
    let failed =
      fun
      [ [symb; sep] -> Fail.symb_failed_txt entry sep symb
      | _ -> "failed" ]
    in
    let parse_top =
      fun
      [ [symb; _] -> Parse.parse_top_symb entry symb (* FIXME context *)
      | _ -> raise Stream.Failure ]
    in
    let rec kont accu =
      parser
      [ [: () = psep;
          a =
            parser
            [ [: a = psymb :] -> a
            | [: a = parse_top symbl :] -> Obj.magic a
            | [: :] -> raise (Stream.Error (failed symbl)) ];
          s :] ->
            kont (f a accu) s
      | [: :] -> accu ]
    in
    parser [: a = psymb; s :] -> kont (f a e) s
  ;
end;
