open Rtype

let rec iter f ty v =
  try
    f ty v;
    if Obj.is_block v then 
      match ty.desc with
      | Tvar -> ()
      | Tarrow (_,_,_) -> ()
      | Ttuple ts -> 
      	  let rec iter_mem pos = function
            | [] -> ()
            | t::ts -> 
      		iter f t (Obj.field v pos);
      		iter_mem (pos+1) ts
      	  in
      	  iter_mem 0 ts
	    
      | Tconstr ((_, decl), args) -> 
	  
	  let sub = List.combine decl.type_params args in
	  
	  let get_fields tys =
	    let rec get_fields p = function
	      | [] -> []
	      | _::xs -> (Obj.field v p) :: get_fields (p+1) xs
	    in
	    get_fields 0 tys
	  in

      	  match decl.type_kind with
      	  | Type_abstract -> 
      	      begin match decl.type_manifest with
      	      | Some ty -> iter f (Rtype.subst sub ty) v
      	      | None -> ()
      	      end

	  | Type_variant ([], _) ->
	      (* exception *)
	      (* FIMXE exception *)
	      ()

      	  | Type_variant (cnstrs, _) -> 
	      let rec find_typs p = function
		| [] -> assert false
		| (_,[]) :: cs -> find_typs p cs
		| (_,args)::cs -> 
		    if p = 0 then args
		    else find_typs (p-1) cs
	      in
	      let tys = find_typs (Obj.tag v) cnstrs in
	      List.iter2 (fun ty field ->
		iter f (Rtype.subst sub ty) field) tys (get_fields tys)

	  | Type_record (labels, Record_regular, _) ->
	      List.iter2 (fun (_,_,ty) field -> 
		iter f (Rtype.subst sub ty )field) labels (get_fields labels)

	  | Type_record (labels, Record_float, _) ->
	      
	      let fields = 
		(Obj.magic (Array.to_list (Obj.obj v : float array)) : Obj.t list)
	      in
	      List.iter2 (fun (_,_,ty) field -> 
		iter f (Rtype.subst sub ty) field) labels fields
  with
  | Exit -> ()

let iter_with_loop f ty v =
  let visited_block = ref [] in
  let f ty v =
    if Obj.is_block v && List.memq v !visited_block then raise Exit
    else begin
      visited_block := v :: !visited_block;
      f ty v
    end 
  in
  iter f ty v
