open Format
open Rtype

type printer = formatter -> Obj.t -> unit

let rec print_list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs -> 
      fprintf ppf "%a%a%a" 
	f x
	sep ()
	(print_list sep f) xs

let print_tuple f ppf v = 
  print_list (fun ppf () -> fprintf ppf ",@ ") f ppf v

let printers =
  [ Builtintypes.int, ((fun _(*[]*) ppf v -> fprintf ppf "%i" (Obj.obj v)) :
			 printer list -> printer)
      ;
    Builtintypes.char, (fun _(*[]*) ppf v -> fprintf ppf "%C" (Obj.obj v));
    Builtintypes.string, (fun _(*[]*) ppf v -> fprintf ppf "%S" (Obj.obj v));
    Builtintypes.float, (fun _(*[]*) ppf v -> fprintf ppf "%F" (Obj.obj v));
    Builtintypes.bool, (fun _(*[]*) ppf v -> fprintf ppf "%B" (Obj.obj v));
    Builtintypes.unit, (fun _(*[]*) ppf v -> fprintf ppf "()");
    Builtintypes.exn, (fun _(*[]*) ppf v -> fprintf ppf "<exn>");
    Builtintypes.nativeint, (fun _(*[]*) ppf v -> fprintf ppf "%nd" (Obj.obj v));
    Builtintypes.int32, (fun _(*[]*) ppf v -> fprintf ppf "%ld" (Obj.obj v));
    Builtintypes.int64, (fun _(*[]*) ppf v -> fprintf ppf "%Ld" (Obj.obj v));
    
    Builtintypes.list, (fun printers ppf v ->
      match printers with
      | [printer] ->
	  fprintf ppf "@[<2>[ %a ]@]" 
	    (print_list (fun ppf () -> fprintf ppf ";@ ") printer) (Obj.obj v)
      | _ -> assert false);

    Builtintypes.array, (fun printers ppf v ->
      match printers with
      | [printer] ->
	  fprintf ppf "@[<2>[| %a |]@]" 
	    (print_list (fun ppf () -> fprintf ppf ";@ ") printer) 
	    (Array.to_list (Obj.obj v))
      | _ -> assert false)
      
  ]

generic val print : {'a} => formatter -> 'a -> unit =
  let rec print =
    fun ty ppf v ->
      match ty.desc with
      | Tvar -> fprintf ppf "<poly>"
      | Tarrow (_,_,_) -> fprintf ppf "<fun>"
      | Ttuple ts -> 
  	(* bind types and values *)
  	let rec bind pos = function
  	  | [] -> []
  	  | t::ts -> (t, Obj.field v pos) :: bind (pos+1) ts
  	in
  	fprintf ppf "(@[%a@])" 
  	  (print_tuple (fun ppf (t,v) -> print t ppf v)) (bind 0 ts)
      | Tconstr ((path, decl), args) -> 
	  let sub_printers = (List.map print args : printer list) in
	  begin try
	    List.assq decl printers sub_printers ppf v
	  with
	  | Not_found -> fprintf ppf "<???>"
	  end
  in
  print
