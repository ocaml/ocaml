##ifdef CAMLTK

(* The function *must* use tkreturn *)
let handle_set opts w cmd =
  tkCommand [|
    TkToken"selection";
    TkToken"handle";
    TkTokenList 
      (List.map 
         (function x -> cCAMLtoTKicccm w icccm_selection_handle_table x)
         opts);
    cCAMLtoTKwidget widget_any_table w;
    let id = register_callback w (function args ->
      let (a1,args) = int_of_string (List.hd args), List.tl args in
      let (a2,args) = int_of_string (List.hd args), List.tl args in
      cmd a1 a2) in
    TkToken ("camlcb "^id)
  |]
;;

##else

(* The function *must* use tkreturn *)
let handle_set ~command =
selection_handle_icccm_optionals (fun opts w ->
  tkCommand [|
    TkToken"selection";
    TkToken"handle";
    TkTokenList opts;
    cCAMLtoTKwidget w;
    let id = register_callback w ~callback:
        begin fun args ->
          let pos = int_of_string (List.hd args) in
          let len = int_of_string (List.nth args 1) in
          tkreturn (command ~pos ~len)
        end
    in TkToken ("camlcb " ^ id)
  |])
;;

##endif
