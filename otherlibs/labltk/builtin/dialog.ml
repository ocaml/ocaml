##ifdef CAMLTK

let create ?name parent title mesg bitmap def buttons =
  let w = Widget.new_atom "toplevel" ~parent ?name in
  let res = tkEval [|TkToken"tk_dialog";
                     cCAMLtoTKwidget widget_any_table w;
                     TkToken title;
                     TkToken mesg;
                     cCAMLtoTKbitmap bitmap;
                     TkToken (string_of_int def);
                     TkTokenList (List.map (function x -> TkToken x) buttons)|]
   in
    int_of_string res
;;

let create_named parent name title mesg bitmap def buttons =
  let w = Widget.new_atom "toplevel" ~parent ~name in
  let res = tkEval [|TkToken"tk_dialog";
                     cCAMLtoTKwidget widget_any_table w;
                     TkToken title;
                     TkToken mesg;
                     cCAMLtoTKbitmap bitmap;
                     TkToken (string_of_int def);
                     TkTokenList (List.map (function x -> TkToken x) buttons)|]
   in
    int_of_string res
;;

##else

let create ~parent ~title ~message ~buttons ?name
    ?(bitmap = `Predefined "") ?(default = -1) () =
  let w = Widget.new_atom "toplevel" ?name ~parent in
  let res = tkEval [|TkToken"tk_dialog";
                     cCAMLtoTKwidget w;
                     TkToken title;
                     TkToken message;
                     cCAMLtoTKbitmap bitmap;
                     TkToken (string_of_int default);
                     TkTokenList (List.map ~f:(fun x -> TkToken x) buttons)|]
   in
    int_of_string res
;;

##endif
