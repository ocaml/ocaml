
type parser_error =
  Unclosed of Location.t * string * string
| Expecting of string

type lexer_error =
  Unmatched_verbatim
| Unmatched_target
| Unmatched_code
| Unmatched_pre_code
| Unmatched_html_code
| Unterminated_verbatim
| Unterminated_target
| Unterminated_code
| Unterminated_pre_code
| Unterminated_ref
| Unterminated_html_code
| Nested_verbatim
| Nested_target
| Nested_pre_code
| Nested_html_code
| Expected_see
| Unterminated_see_url
| Unterminated_see_file
| Unterminated_see_doc
| Expected_ident
| Expected_name
| Expected_version
| Expected_exception

type comment_error =
  Unterminated_simple
| Unterminated_special

type error =
  Lexer of lexer_error
| Parser of parser_error
| Comments of comment_error

exception Error of Location.t * error

let lexer_error_message = function
  | Unmatched_verbatim ->
      "Documentation syntax error: unmatched 'v}'"
  | Unmatched_target ->
      "Documentation syntax error: unmatched '%%}'"
  | Unmatched_code ->
      "Documentation syntax error: unmatched ']'"
  | Unmatched_pre_code ->
      "Documentation syntax error: unmatched ']}'"
  | Unmatched_html_code ->
      "Documentation syntax error: unmatched '</code>'"
  | Unterminated_verbatim ->
      "Documentation syntax error: unterminated '{v'"
  | Unterminated_target ->
      "Documentation syntax error: unterminated '{%%'"
  | Unterminated_code ->
      "Documentation syntax error: unterminated '['"
  | Unterminated_pre_code ->
      "Documentation syntax error: unterminated '{['"
  | Unterminated_ref ->
      "Documentation syntax error: unterminated '{!'"
  | Unterminated_html_code ->
      "Documentation syntax error: unterminated '<code>'"
  | Nested_verbatim ->
      "Documentation syntax error: nested '{v'"
  | Nested_target ->
      "Documentation syntax error: nested '{%%'"
  | Nested_pre_code ->
      "Documentation syntax error: nested '{['"
  | Nested_html_code ->
      "Documentation syntax error: nested '<code>'"
  | Expected_see ->
      "Documentation syntax error: expected < url >, 'filename' or \"document\""
  | Unterminated_see_url ->
      "Documentation syntax error: unterminated url"
  | Unterminated_see_file ->
      "Documentation syntax error: unterminated filename"
  | Unterminated_see_doc ->
      "Documentation syntax error: unterminated document name"
  | Expected_ident ->
      "Documentation syntax error: expected identifier"
  | Expected_name ->
      "Documentation syntax error: expected author name"
  | Expected_version ->
      "Documentation syntax error: expected version string"
  | Expected_exception ->
      "Documentation syntax error: expected exception name"

let parser_error_message = function
  | Unclosed(opening_loc, opening, closing) ->
      "Documentation syntax error: '" ^ closing ^ "' expected"
  | Expecting nonterm ->
      "Documentation syntax error: " ^ nonterm ^ " expected"

let comments_error_message = function
  | Unterminated_simple -> "Documentation syntax error: unterminated comment"
  | Unterminated_special -> "Documentation syntax error: unterminated special comment"

let error_message = function
  | Lexer x -> lexer_error_message x
  | Parser x -> parser_error_message x
  | Comments x -> comments_error_message x
