let cCAMLtoTKpaletteType : paletteType -> tkArgs = function
    `Gray (foo) -> TkToken (string_of_int foo)
  | `Rgb (r,v,b) -> TkToken (string_of_int r^"/"^
				  string_of_int v^"/"^
				  string_of_int b)

