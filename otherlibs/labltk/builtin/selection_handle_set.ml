(* The function *must* use tkreturn *)
let handle_set command: cmd =
selection_handle_icccm_optionals (fun opts w ->
  tkCommand [|TkToken"selection";
              TkToken"handle";
              TkTokenList opts;
              cCAMLtoTKwidget w;
              let id = register_callback w callback:(function args ->
                let a1 = int_of_string (List.hd args) in
                let a2 = int_of_string (List.nth args 1) in
                tkreturn (cmd pos:a1 len:a2)) in TkToken ("camlcb " ^ id)
            |])

