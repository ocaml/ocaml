(* TEST *)

(*

A test file for Format.pp_print_custom_break.

*)
let fprintf, printf, list = Format.(fprintf, printf, pp_print_list)
let string, custom_break = Format.(pp_print_string, pp_print_custom_break)

let () = Format.set_margin 30

let example = [
  "Foo"; "Baz"; "Bar"; "Qux"; "Quux"; "Quuz"; "Corge"; "Grault"; "Garply";
]

let boxes = ["v"; "b"; "h"; "hv"; "hov"]

let test format data =
  boxes |> List.iter (fun box ->
    printf "## The %S box@\n```@\n@[<%s 0>%a@]@\n```@\n@\n" box box
      (format box) data);

module Format_list = struct
  let pp_sep ppf () = fprintf ppf ";@ "

  let format box_type ppf items =
    fprintf ppf "[@;<0 2>@[<%s>%a@]%t]" box_type
      (list ~pp_sep string) items
      (custom_break ~fits:("", 0, "") ~breaks:(";", 0, ""))

  let () =
    printf "# Printing arrays: last trailing semicolon is optional@\n@\n";
    test format example
end


module Format_statements = struct
  let pp_sep ppf () =
    custom_break ppf ~fits:(";", 1, "") ~breaks:("", 0, "")

  let rec format box_type ppf items =
    fprintf ppf "{@;<0 2>@[<%s>%a@]@,}" box_type
      (list ~pp_sep string) items

  let () =
    printf "# Printing statements: terminator is optional after newline@\n@\n";
    test format example
end


module Format_function = struct
  let pp_sep ppf () = fprintf ppf "@ | "
  let format_case ppf = fprintf ppf "%s -> ()"

  let rec format box_type ppf items =
    fprintf ppf "@[<%s>function%t%a@]" box_type
      (custom_break ~fits:("", 1, "") ~breaks:("", 0, "| "))
      (list ~pp_sep format_case) items

  let () =
    printf "# Printing function: first pipe character is optional@\n@\n";
    test format example
end
