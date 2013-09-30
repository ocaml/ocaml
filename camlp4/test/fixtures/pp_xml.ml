
type xml = Elt of string * xml list | Pcdata of string

let pp = Format.fprintf

let rec print_elt f =
  function
  | Elt (tag, contents) ->
      pp f "@[<hv0>@[<hv2><%s>@,%a@]@,</%s>@]"
        tag print_list_elts contents tag
  | Pcdata s ->
      Format.pp_print_string f s

and print_list_elts f =
    let rec loop =
      function
      | [] -> ()
      | x::xs -> (pp f "@,"; print_elt f x; loop xs) in
    function
    | [] -> ()
    | [x] -> print_elt f x
    | x::xs -> (print_elt f x; loop xs)

let tree =
  Elt ("div", [
     Elt ("p", [Pcdata "a short text"]);
     Elt ("p", [Pcdata "a looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong text"])
  ])

let () = Format.printf "%a@." print_elt tree
