(* The function *must* use tkreturn *)
let handle_set ~command =
selection_handle_icccm_optionals (fun opts w ->
  tkCommand [|TkToken"selection";
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

