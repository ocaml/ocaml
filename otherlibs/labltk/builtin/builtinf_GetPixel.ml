let pixels units =
  let res =
    tkEval
     [|TkToken"winfo";
       TkToken"pixels";
       cCAMLtoTKwidget default_toplevel;
       cCAMLtoTKunits units|] in 
  int_of_string res
