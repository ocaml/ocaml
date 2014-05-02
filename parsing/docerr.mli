
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

val error_message: error -> string
