function
| Some Some Some x -> x
(* | None None None x -> x *)
| _ -> assert false;;

fun None None None -> ();;

fun (Some None) None None -> ();;

fun Some None None None -> ();;
