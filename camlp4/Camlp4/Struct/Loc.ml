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
(* camlp4r *)

open Format;

(* FIXME
   Study these 2 others implementations which change the ghost
   handling:

   type pos = ... the same ...
   
   1/
   
   type loc = {
     file_name : string;
     start     : pos;
     stop      : pos
   };
   
   type t =
     [ Nowhere
     | Ghost of loc (* the closest non ghost loc *)
     | Concrete of loc ];
   
   2/
   
   type loc = {
     file_name : string;
     start     : pos;
     stop      : pos
   };
   
   type t = option loc;
   
   3/
   
   type t = {
     file_name : option string;
     start     : pos;
     stop      : pos
   };

*)

type pos = {
  line : int;
  bol  : int;
  off  : int
};

type t = {
  file_name : string;
  start     : pos;
  stop      : pos;
  ghost     : bool
};

(* Debug section *)
value dump_sel f x =
  let s =
    match x with
    [ `start -> "`start"
    | `stop  -> "`stop"
    | `both  -> "`both"
    | _      -> "<not-printable>" ]
  in pp_print_string f s;
value dump_pos f x =
  fprintf f "@[<hov 2>{ line = %d ;@ bol = %d ;@ off = %d } : pos@]"
          x.line x.bol x.off;
value dump_long f x =
  fprintf f
    "@[<hov 2>{ file_name = %s ;@ start = %a (%d-%d);@ stop = %a (%d);@ ghost = %b@ } : Loc.t@]"
    x.file_name dump_pos x.start (x.start.off - x.start.bol)
    (x.stop.off - x.start.bol) dump_pos x.stop
    (x.stop.off - x.stop.bol) x.ghost;
value dump f x =
  fprintf f "[%S: %d:%d-%d %d:%d%t]"
    x.file_name x.start.line (x.start.off - x.start.bol)
    (x.stop.off - x.start.bol) x.stop.line (x.stop.off - x.stop.bol)
    (fun o -> if x.ghost then fprintf o " (ghost)" else ());

value start_pos = { line = 1 ; bol = 0 ; off = 0 };

value ghost =
  { file_name = "ghost-location";
    start     = start_pos;
    stop      = start_pos;
    ghost     = True     };

value mk file_name =
  debug loc "mk %s@\n" file_name in
  { file_name = file_name;
    start     = start_pos;
    stop      = start_pos;
    ghost     = False    };

value of_tuple (file_name, start_line, start_bol, start_off,
                          stop_line,  stop_bol,  stop_off, ghost) =
  { file_name = file_name;
    start     = { line = start_line ; bol = start_bol ; off = start_off };
    stop      = { line = stop_line  ; bol = stop_bol  ; off = stop_off  };
    ghost     = ghost };

value to_tuple
  { file_name = file_name;
    start     = { line = start_line ; bol = start_bol ; off = start_off };
    stop      = { line = stop_line  ; bol = stop_bol  ; off = stop_off  };
    ghost     = ghost } =
  (file_name, start_line, start_bol, start_off,
              stop_line,  stop_bol,  stop_off, ghost);

value pos_of_lexing_position p =
  let pos =
  { line = p.Lexing.pos_lnum ;
    bol  = p.Lexing.pos_bol  ;
    off  = p.Lexing.pos_cnum } in
  debug loc "pos_of_lexing_position: %a@\n" dump_pos pos in
  pos;

value pos_to_lexing_position p file_name =
  (* debug loc "pos_to_lexing_position: %a@\n" dump_pos p in *)
  { Lexing.
    pos_fname = file_name;
    pos_lnum  = p.line   ;
    pos_bol   = p.bol    ;
    pos_cnum  = p.off    };

value better_file_name a b =
  match (a, b) with
  [ ("", "") -> a
  | ("", x)  -> x
  | (x, "")  -> x
  | ("-", x) -> x
  | (x, "-") -> x
  | (x, _)   -> x ];

value of_lexbuf lb =
  let start = Lexing.lexeme_start_p lb
  and stop  = Lexing.lexeme_end_p lb in
  let loc =
  { file_name = better_file_name start.Lexing.pos_fname stop.Lexing.pos_fname;
    start     = pos_of_lexing_position start;
    stop      = pos_of_lexing_position stop;
    ghost     = False } in
  debug loc "of_lexbuf: %a@\n" dump loc in
  loc;

value of_lexing_position pos =
  let loc =
  { file_name = pos.Lexing.pos_fname;
    start     = pos_of_lexing_position pos;
    stop      = pos_of_lexing_position pos;
    ghost     = False } in
  debug loc "of_lexing_position: %a@\n" dump loc in
  loc;

value to_ocaml_location x =
  debug loc "to_ocaml_location: %a@\n" dump x in
  { Camlp4_import.Location.
    loc_start = pos_to_lexing_position x.start x.file_name;
    loc_end   = pos_to_lexing_position x.stop x.file_name;
    loc_ghost = x.ghost };

value of_ocaml_location { Camlp4_import.Location.loc_start = a; loc_end = b; loc_ghost = g } =
  let res =
    { file_name = better_file_name a.Lexing.pos_fname b.Lexing.pos_fname;
      start     = pos_of_lexing_position a;
      stop      = pos_of_lexing_position b;
      ghost     = g } in
  debug loc "of_ocaml_location: %a@\n" dump res in
  res;

value start_pos x = pos_to_lexing_position x.start x.file_name;
value stop_pos x = pos_to_lexing_position x.stop x.file_name;

value merge a b =
  if a == b then
    debug loc "trivial merge@\n" in
    a
  else
    let r = 
      match (a.ghost, b.ghost) with
      [ (False, False) ->
        (* FIXME if a.file_name <> b.file_name then
          raise (Invalid_argument
            (sprintf "Loc.merge: Filenames must be equal: %s <> %s"
                    a.file_name b.file_name))                          *)
        (* else *)
          { (a) with stop = b.stop }
      | (True, True) -> { (a) with stop = b.stop }
      | (True, _) -> { (a) with stop = b.stop }
      | (_, True) -> { (b) with start = a.start } ]
    in debug loc "@[<hov 6>merge %a@ %a@ %a@]@\n" dump a dump b dump r in r;

value join x = { (x) with stop = x.start };

value map f start_stop_both x =
  match start_stop_both with
  [ `start -> { (x) with start = f x.start }
  | `stop  -> { (x) with stop  = f x.stop }
  | `both  -> { (x) with start = f x.start; stop  = f x.stop } ];

value move_pos chars x = { (x) with off = x.off + chars };

value move s chars x =
  debug loc "move %a %d %a@\n" dump_sel s chars dump x in
  map (move_pos chars) s x;

value move_line lines x =
  debug loc "move_line %d %a@\n" lines dump x in
  let move_line_pos x =
    { (x) with line = x.line + lines ; bol = x.off }
  in map move_line_pos `both x;

value shift width x =
  { (x) with start = x.stop ; stop = move_pos width x.stop };

value file_name  x = x.file_name;
value start_line x = x.start.line;
value stop_line  x = x.stop.line;
value start_bol  x = x.start.bol;
value stop_bol   x = x.stop.bol;
value start_off  x = x.start.off;
value stop_off   x = x.stop.off;
value is_ghost   x = x.ghost;

value set_file_name s x =
  debug loc "set_file_name: %a@\n" dump x in
  { (x) with file_name = s };

value ghostify x =
  debug loc "ghostify: %a@\n" dump x in
  { (x) with ghost = True };

value make_absolute x =
  debug loc "make_absolute: %a@\n" dump x in
  let pwd = Sys.getcwd () in
  if Filename.is_relative x.file_name then
    { (x) with file_name = Filename.concat pwd x.file_name }
  else x;

value strictly_before x y =
  let b = x.stop.off < y.start.off && x.file_name = y.file_name in
  debug loc "%a [strictly_before] %a => %b@\n" dump x dump y b in
  b;

value to_string x = do {
  let (a, b) = (x.start, x.stop) in
  let res = sprintf "File \"%s\", line %d, characters %d-%d"
                    x.file_name a.line (a.off - a.bol) (b.off - a.bol) in
  if x.start.line <> x.stop.line then
    sprintf "%s (end at line %d, character %d)"
            res x.stop.line (b.off - b.bol)
  else res
};

value print out x = pp_print_string out (to_string x);

value check x msg =
  if ((start_line x) > (stop_line x) ||
      (start_bol x) > (stop_bol x) ||
      (start_off x) > (stop_off x) ||
      (start_line x) < 0 || (stop_line x) < 0 ||
      (start_bol x) < 0 || (stop_bol x) < 0 ||
      (start_off x) < 0 ||  (stop_off x) < 0)
      (* Here, we don't check
        (start_off x) < (start_bol x) || (stop_off x) < (start_bol x)
        since the lexer is called on antiquotations, with off=0, but line and bolpos
        have "correct" values *)
  then do {
    eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
    False
  }
  else True;

exception Exc_located of t and exn;

ErrorHandler.register
  (fun ppf ->
    fun [ Exc_located loc exn ->
            fprintf ppf "%a:@\n%a" print loc ErrorHandler.print exn
        | exn -> raise exn ]);

value name = ref "_loc";

value raise loc exc =
  match exc with
  [ Exc_located _ _ -> raise exc
  | _ -> raise (Exc_located loc exc) ]
;
