{
let need_space =
  ref false;;

let addspace () =
  if !need_space then begin print_char ` `; need_space := false end;;
}

rule main = parse
    "\\begin{syntax}" {
      print_string "\\begin{rawhtml}\n<PRE>\n";
      need_space := false;
      syntax lexbuf;
      print_string "</PRE>\n\\end{rawhtml}\n";
      main lexbuf }
  | "\\@" {
      print_string "@";
      main lexbuf }
  | "@" {
      print_string "%\n\\begin{rawhtml}";
      need_space := false;
      syntax lexbuf;
      print_string "\\end{rawhtml}%\n";
      main lexbuf }
  | _ {
      print_char (get_lexeme_char lexbuf 0); main lexbuf }
  | eof {
      () }

and syntax = parse
    "\\end{syntax}" { () }
  | "@" { () }
  | `'` {
      addspace();
      print_string "<code>";
      inquote lexbuf;
      print_string "</code>";
      need_space := true;
      syntax lexbuf }
  | `"` {
      addspace();
      print_string "<code>";
      indoublequote lexbuf;
      print_string "</code>";
      need_space := true;
      syntax lexbuf }
  | [`a`-`z``-`] + {
      addspace();
      print_string "<i>";
      print_string (get_lexeme lexbuf);
      print_string "</i>";
      need_space := true;
      syntax lexbuf }
  | `\\` [`a`-`z``A`-`Z`] + {
      begin match get_lexeme lexbuf with
        "\\ldots" -> print_string "..."; need_space := false
      | s -> printf__eprintf "Warning: %s ignored.\n" s
      end;
      syntax lexbuf }
  | [`_` `^`] _ {
      let subscript = get_lexeme_char lexbuf 1 in
      if subscript >= `a` & subscript <= `z`
      then print_char(char_of_int(int_of_char subscript - 32))
      else print_char subscript;
      syntax lexbuf }
  | ":" {
      print_string ":\n      ";
      need_space := false;
      syntax lexbuf }
  | "|" {
      print_string "\n   |  ";
      need_space := false;
      syntax lexbuf }
  | ";" {
      print_string "\n\n";
      need_space := false;
      syntax lexbuf }
  | [ `{` `[` `(`] {
      addspace(); print_string "<i>"; print_string (get_lexeme lexbuf);
      print_string "</i>"; syntax lexbuf }
  | [ `}` `]` `)`] {
      print_string "<i>"; print_string (get_lexeme lexbuf);
      print_string "</i>"; syntax lexbuf }
  | "{{" {
      addspace(); print_string "<i>{</i>"; syntax lexbuf }
  | "}}" {
      print_string "<i>}+</i>"; syntax lexbuf }
  | "||" {
      print_string " | "; need_space := false; syntax lexbuf }
  | [ ` ` `\n` `\t` `~`] {
      syntax lexbuf }
  | [ `,` ] {
      print_char(get_lexeme_char lexbuf 0); syntax lexbuf }
  | _ {
      printf__eprintf "Warning: %s ignored at char %d.\n"
                      (get_lexeme lexbuf) (get_lexeme_start lexbuf);
      syntax lexbuf }

and inquote = parse
    `'` { () }
  | `&` { print_string "&amp;"; inquote lexbuf }
  | `<` { print_string "&lt;"; inquote lexbuf }
  | `>` { print_string "&gt;"; inquote lexbuf }
  | _   { print_char (get_lexeme_char lexbuf 0); inquote lexbuf }

and indoublequote = parse
    `"` { () }
  | `&` { print_string "&amp;"; indoublequote lexbuf }
  | `<` { print_string "&lt;"; indoublequote lexbuf }
  | `>` { print_string "&gt;"; indoublequote lexbuf }
  | _   { print_char (get_lexeme_char lexbuf 0); indoublequote lexbuf }
;;
