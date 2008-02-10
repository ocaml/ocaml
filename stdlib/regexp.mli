type token

type typ = {
    num_of_groups : int;
    named_groups : (string * int) list;
    callouts : int list; (* 255 : default, not in the list *)
    named_callouts : (string * int) list; (* caml extension *)
  }

val from_string : string -> token list
val type_regexp : token list -> typ

val string_of_tokens : typ -> token list -> string 

class virtual result : typ -> string array -> object
  method _groups : string array
  method _named_groups : (string * string) list
  method _group : int -> string
  method _unsafe_group : int -> string
  method _named_group : string -> string
end

type 'a t
