(* builtin to handle callback association to widget *)
let own_set ?:command =
selection_ownset_icccm_optionals ?:command (fun opts w ->
tkEval [|TkToken"selection";
	 TkToken"own";
	 TkTokenList 
      	   (List.map 
      	     fun:(function x -> 
      	       cCAMLtoTKselection_ownset_icccm w x)
      	     opts);
	cCAMLtoTKwidget w|];
())

