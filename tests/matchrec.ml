type t = {{ <a>[ <b>[]t* <b>[]] }}

(* This function transform all <b> elements with an <x>[] element. *)

let f (x : {{ [ t ] }}) = {{ map* x with <b>_ -> [ <x>[] ] }}

let () = print_endline (Xml_values.to_string  {{f [ <a>[ <b>[] <b>[] ] ]}})
