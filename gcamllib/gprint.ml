open Format

let rec print_list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "%a%a%a" 
	f x
	sep ()
	(print_list sep f) xs

let print_tuple f ppf = print_list (fun ppf () -> fprintf ppf "@,*@,") f ppf

generic val print : {'a} => formatter -> 'a -> unit =
  fun ty ppf v ->
    match ty.desc with
    | Tvar -> fprintf ppf "<poly>"
    | Tarrow (_,_,_) -> fprintf ppf "<fun">
    | Ttuple ts -> print_tuple print ppf v
