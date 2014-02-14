type location = Lexing.position * Lexing.position

val print_loc : Format.formatter -> location -> unit
val print_loc_option : Format.formatter -> location option -> unit

val of_lexbuf : Lexing.lexbuf -> location
