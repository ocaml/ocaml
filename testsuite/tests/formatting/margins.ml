(* TEST
   * toplevel
*)

let () = Format.pp_set_margin Format.std_formatter 20;;

1 + "foo";;

let () = Format.pp_set_margin Format.std_formatter 80;;
let () = Format.pp_set_max_indent Format.std_formatter 70;;

1 + "foo";;
