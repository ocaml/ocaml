let create :parent :title :message :buttons ?:name
    ?:bitmap{=`Predefined ""} ?:default{= -1} () =
  let w = Widget.new_atom "toplevel" ?:name :parent in
  let res = tkEval [|TkToken"tk_dialog";
		     cCAMLtoTKwidget w;
		     TkToken title;
		     TkToken message;
	             cCAMLtoTKbitmap bitmap;
	             TkToken (string_of_int default);
	             TkTokenList (List.map fun:(fun x -> TkToken x) buttons)|]
   in
    int_of_string res
