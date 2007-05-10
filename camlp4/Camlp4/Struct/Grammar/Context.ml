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

module type S = sig
  module Token : Sig.Token;
  open Token;
  type t;
  value call_with_ctx : Stream.t (Token.t * Loc.t) -> (t -> 'a) -> 'a;
  value loc_bp : t -> Loc.t;
  value loc_ep : t -> Loc.t;
  value stream : t -> Stream.t (Token.t * Loc.t);
  value peek_nth : t -> int -> option (Token.t * Loc.t);
  value njunk : t -> int -> unit;
  value junk : Stream.t (Token.t * Loc.t) -> unit;
  value bp : Stream.t (Token.t * Loc.t) -> Loc.t;
end;

module Make (Token : Sig.Token) : S with module Token = Token = struct
  module Token = Token;
  open Token;

  type t = { strm : mutable Stream.t (Token.t * Loc.t);
             loc  : mutable Loc.t };

  value loc_bp c = 
    match Stream.peek c.strm with
    [ None -> Loc.ghost
    | Some (_, loc) -> loc ];

  value loc_ep c = c.loc;

  value set_loc c =
    match Stream.peek c.strm with
    [ Some (_, loc) -> c.loc := loc
    | None -> () ];

  value mk strm =
    match Stream.peek strm with
    [ Some (_, loc) -> { strm = strm; loc = loc }
    | None -> { strm = strm ; loc = Loc.ghost } ];

  value stream c = c.strm;

  value peek_nth c n =
    let list = Stream.npeek n c.strm in
    let rec loop list n =
      match (list, n) with
      [ ([((_, loc) as x) :: _], 1) -> do { c.loc := loc; Some x }
      | ([_ :: l], n) -> loop l (n - 1)
      | ([], _) -> None ]
    in
    loop list n;

  value njunk c n =
    (for i = 1 to n do Stream.junk c.strm done;
     set_loc c);

  value streams = ref [];
  value mk strm =
    let c = mk strm in
    let () = streams.val := [(strm, c) :: streams.val] in c;
  value junk strm =
    do { set_loc (List.assq strm streams.val); Stream.junk strm };
  value bp strm = loc_bp (List.assq strm  streams.val);

   value call_with_ctx strm f =
     let streams_v = streams.val in
     let r =
       try f (mk strm) with exc -> do { streams.val := streams_v; raise exc }
     in
     do { streams.val := streams_v; r }
   ;

end;
