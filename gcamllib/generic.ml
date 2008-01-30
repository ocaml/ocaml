open Gcaml
open Rtype

(* iter *)

let rec iter cache f d =
  let ty = type_of_dyn d in
  let obj = obj_of_dyn d in

  try
    if Obj.is_block obj then begin
      if List.memq obj !cache then () (* already visited *) 
      else raise Not_found
    end else raise Not_found
  with Not_found ->

  cache := obj :: !cache;

  try 
    f (iter cache f) d; (* raising Exit inside f cancells further iteration *)
    
    if not (Obj.is_block obj) then ()
    else
      let tag = Obj.tag obj in
      let get_fields ts =
  	let rec extract pos = function
  	  | [] -> []
  	  | t::ts -> dyn_of t (Obj.field obj pos) :: extract (pos + 1) ts
  	in
  	extract 0 ts
      in
      match ty.desc with
      | Tvar -> ()
      | Tarrow (_,_,_) -> ()
      | Ttuple ts -> 
  	  assert (tag = 0);
  	  List.iter (iter cache f) (get_fields ts)
      | Tconstr ((_, Box decl), args) -> 
    	  let sub = List.combine decl.type_params args in
  	  match decl.type_kind with
  	  | Type_abstract -> 
              begin match decl.type_manifest with
              | Some ty' ->
          	  let ty' = Rtype.subst sub ty' in
		  (* cancel the latest cache, since we exec it again *)
		  cache := List.tl !cache;
          	  iter cache f (dyn_of ty' obj)
              | None -> ()
              end
  		    
    	  | Type_variant ([], _) ->
    	      (* exception *)
    	      (* FIMXE exception *)
  	      ()
  	  | Type_variant (cnstrs, _) -> 
    	      let tys = 
    		let rec find_typs p = function
    		  | [] -> assert false
    		  | (_,[]) :: cs -> find_typs p cs
    		  | (_,typs)::cs -> 
    		      if p = 0 then List.map (Rtype.subst sub) typs
    		      else find_typs (p-1) cs
    		in
  		find_typs tag cnstrs 
  	      in
  	      List.iter (iter cache f) (get_fields tys)
  		    
    	  | Type_record (fields, Record_regular, _) ->
  	      assert (tag = 0); 
    	      let tys = List.map (fun (_,_,ty) -> Rtype.subst sub ty) fields in
  	      List.iter (iter cache f) (get_fields tys)
  		    
    	  | Type_record (labels, Record_float, _) ->
  	      assert (tag = 0); 
    	      let tys = List.map (fun (_,_,ty) -> Rtype.subst sub ty) labels in
  	      let fields =
  		let list = 
  		  Array.to_list (Obj.obj obj : float array) 
  		in
  		List.map2 (fun ty f -> dyn_of ty (Obj.repr f))
  		  tys list
  	      in
  	      List.iter (iter cache f) fields
  with
  | Exit -> ()

let iter f d =
  let cache = ref [] in
  iter cache f d

(* generic map

 - The orignal is kept as it is.   
 - The result may share blocks with the original
 - Shared blocks in the original are also shared in the result.
     (i.e. [f] is applied at most once for each physical block)
 - Loops may fail the mapping.
 - If terminated successfully, loops in the original are mapped 
     to loops in the result.
 
 *)

let rec map cache f d =
  let ty = type_of_dyn d in
  let obj = obj_of_dyn d in

  let result_slot = ref None in
  let add_cache obj' = if Obj.is_block obj then result_slot := Some obj' in

  try
    if Obj.is_block obj then begin
      match !(List.assq obj !cache) with
      | Some obj' -> dyn_of ty obj'
      | None -> 
	  Format.eprintf "LOOP: %a@." Rtype.print ty;
	  raise (Failure "failed to map a loop")
    end else raise Not_found
  with Not_found ->

  cache := (obj, result_slot) :: !cache;

  let d' =
    try
      match f (map cache f) d with
      | Some d' -> d'
      | None ->
  	  if not (Obj.is_block obj) then d
  	  else
  	    let tag = Obj.tag obj in
  	    let get_fields ts =
  	    let rec extract pos = function
  	      | [] -> []
  	      | t::ts -> dyn_of t (Obj.field obj pos) :: extract (pos + 1) ts
  	    in
  	    extract 0 ts
  	  in
  	  let make_block tag ds =
    	    let blk = Obj.new_block tag (List.length ds) in
	    add_cache blk;
  	    let rec fill pos = function
  	      | [] -> ()
  	      | d::ds -> 
  		  Obj.set_field blk pos (obj_of_dyn d);
  		  fill (pos+1) ds
  	    in
  	    fill 0 ds;
  	    dyn_of ty blk
  	  in
  	  let make_floats ds =
    	    let ary = Array.create (List.length ds) 0. in
	    add_cache (Obj.repr ary);
  	    let rec fill pos = function
  	      | [] -> ()
  	      | d::ds -> 
  		  ary.(pos) <- Obj.obj (obj_of_dyn d);
  		  fill (pos+1) ds
  	    in
  	    fill 0 ds;
  	    dyn_of ty (Obj.repr ary)
  	  in
  
    	  match ty.desc with
    	  | Tvar -> d
    	  | Tarrow (_,_,_) -> d
    	  | Ttuple ts -> 
  	      assert (tag = 0);
  	      make_block tag (List.map (map cache f) (get_fields ts))
  	  | Tconstr ((_, Box decl), args) -> 
    	      let sub = List.combine decl.type_params args in
  	      match decl.type_kind with
  	      | Type_abstract -> 
          	  begin match decl.type_manifest with
          	  | Some ty' ->
          	      let ty' = Rtype.subst sub ty' in
		      (* cancel the latest cache, since we exec it again *)
		      cache := List.tl !cache;
          	      let d' = map cache f (dyn_of ty' obj) in
		      (* modify the type *)
		      dyn_of ty (obj_of_dyn d')
          	  | None -> d
          	  end
  		    
    	      | Type_variant ([], _) ->
    		  (* exception *)
    		  (* FIMXE exception *)
  		  d
  	      | Type_variant (cnstrs, _) -> 
    		  let tys = 
    		    let rec find_typs p = function
    		      | [] -> assert false
    		      | (_,[]) :: cs -> find_typs p cs
    		      | (_,typs)::cs -> 
    			  if p = 0 then List.map (Rtype.subst sub) typs
    			  else find_typs (p-1) cs
    		    in
  		    find_typs tag cnstrs 
  		  in
  		  make_block tag (List.map (map cache f) (get_fields tys))
  		    
    	      | Type_record (fields, Record_regular, _) ->
  		  assert (tag = 0); 
    		  let tys =
    		    List.map (fun (_,_,ty) -> Rtype.subst sub ty) fields
  		  in
  		  make_block tag (List.map (map cache f) (get_fields tys))
  		    
    	      | Type_record (labels, Record_float, _) ->
  		  assert (tag = 0); 
    		  let tys =
    		    List.map (fun (_,_,ty) -> Rtype.subst sub ty) labels
  		  in
  		  let fields =
  		    let list = 
  		      Array.to_list (Obj.obj obj : float array) 
  		    in
  		    List.map2 (fun ty f -> dyn_of ty (Obj.repr f))
  		      tys list
  		  in
  		  make_floats (List.map (map cache f) fields)
    with
    | Exit -> d
  in
(*
begin if 
  not (Rtype.equal ty (type_of_dyn d'))
then
  Format.eprintf "%a, %a@." Rtype.print ty Rtype.print (type_of_dyn d')
end;
*)
  assert (Rtype.equal ty (type_of_dyn d')); 
  begin match !result_slot with
  | None -> add_cache (obj_of_dyn d')
  | Some _ -> ()
  end;
  d'

let map f d =
  let cache = ref [] in
  map cache f d
