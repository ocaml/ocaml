##ifdef CAMLTK

let pixels units =
  let res =
    tkEval
     [|TkToken"winfo";
       TkToken"pixels";
       cCAMLtoTKwidget widget_any_table default_toplevel;
       cCAMLtoTKunits units|] in 
  int_of_string res

##else

let pixels units =
  let res =
    tkEval
     [|TkToken"winfo";
       TkToken"pixels";
       cCAMLtoTKwidget default_toplevel;
       cCAMLtoTKunits units|] in 
  int_of_string res

##endif
