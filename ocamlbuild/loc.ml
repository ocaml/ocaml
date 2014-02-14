(* it's not worth adding a dependency on parsing/location.ml(i) or
   compilerlibs just to support location printing, so we re-implement
   that here *)

open Lexing

type location = position * position

let file loc = loc.pos_fname
let line loc = loc.pos_lnum
let char loc = loc.pos_cnum - loc.pos_bol

let print_loc ppf (start, end_) =
  let open Format in
  let print one_or_two ppf (start_num, end_num) =
    if one_or_two then fprintf ppf " %d" start_num
    else fprintf ppf "s %d-%d" start_num end_num in
  fprintf ppf "File %S, line%a, character%a:@."
    (file start)
    (print (line start = line end_))
      (line start, line end_)
    (print (line start = line end_ && char start = char end_))
      (char start, char end_)

let of_lexbuf lexbuf =
  (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let print_loc_option ppf = function
  | None -> ()
  | Some loc -> print_loc ppf loc
