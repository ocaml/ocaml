##ifdef CAMLTK

(* builtin to handle callback association to widget *)
let own_set v1 v2 =
  tkCommand [|
    TkToken"selection";
    TkToken"own";
    TkTokenList 
      (List.map 
         (function x -> cCAMLtoTKicccm v2 icccm_selection_ownset_table x)
         v1);
    cCAMLtoTKwidget widget_any_table v2
  |]
;;

##else

(* builtin to handle callback association to widget *)
let own_set ?command =
  selection_ownset_icccm_optionals ?command (fun opts w ->
    tkCommand [|
      TkToken"selection";
      TkToken"own";
      TkTokenList opts;
      cCAMLtoTKwidget w 
  |])
;;

##endif
