(* Lexer.line_directive_hook tests: check what arrives at
 * line_directive_hook for a few line directives.
 *)

open Location
open Lexing

let source_string ="\

# 5 \"b.ml\"
type a = int

# 8
let b = 5

# 2 \"c.ml\"
let cppo_inc_a = 2
"

let line_directives = ref []

let line_dir_hook loc file_opt line =
  line_directives := (loc, file_opt, line) :: !line_directives

let lex_buf = Lexing.from_string source_string
;;

Location.init lex_buf "a.ml";

assert(!Lexer.line_directive_hook = None);

Lexer.line_directive_hook := Some line_dir_hook;

ignore(Parse.implementation lex_buf);

(*
 * List.iter
 *   (fun (loc, file_opt, line) ->
 *    Printf.eprintf
 *      "\n%s L:%d BOL:%d C:%d - %s L:%d BOL:%d C:%d %b DL:%d DF:%s\n%!"
 *      loc.loc_start.pos_fname
 *      loc.loc_start.pos_lnum
 *      loc.loc_start.pos_bol
 *      loc.loc_start.pos_cnum
 *      loc.loc_end.pos_fname
 *      loc.loc_end.pos_lnum
 *      loc.loc_end.pos_bol
 *      loc.loc_end.pos_cnum
 *      loc.loc_ghost
 *      line
 *      (match file_opt with
 *      | Some f -> f
 *      | None -> "(NONE)")
 *   )
 *   !line_directives;
 *)

assert(List.rev !line_directives =
         [({loc_start = {
               pos_fname="a.ml";
               pos_lnum = 2;
               pos_bol  = 1;
               pos_cnum = 1;};
            loc_end = {
              pos_fname="a.ml";
              pos_lnum = 2;
              pos_bol  = 1;
              pos_cnum = 12;};
            loc_ghost = false;
           },
           (Some "b.ml"),
           5);
          ({loc_start = {
               pos_fname="b.ml";
               pos_lnum = 7;
               pos_bol  = 26;
               pos_cnum = 26;};
            loc_end = {
              pos_fname="b.ml";
              pos_lnum = 7;
              pos_bol  = 26;
              pos_cnum = 30;};
            loc_ghost = false;
           },
           None,
           8);
          ({loc_start = {
               pos_fname="b.ml";
               pos_lnum = 10;
               pos_bol  = 41;
               pos_cnum = 41;};
            loc_end = {
              pos_fname="b.ml";
              pos_lnum = 10;
              pos_bol  = 41;
              pos_cnum = 52;};
            loc_ghost = false;
           },
           (Some "c.ml"),
           2);
         ]
      );

print_endline "OK"
