##ifdef CAMLTK

let cCAMLtoTKunits = function
    Pixels (foo) -> TkToken (string_of_int foo)
  | Millimeters (foo)  -> TkToken(Printf.sprintf "%gm" foo)
  | Inches (foo)  -> TkToken(Printf.sprintf "%gi" foo)
  | PrinterPoint (foo) -> TkToken(Printf.sprintf "%gp" foo)
  | Centimeters (foo) -> TkToken(Printf.sprintf "%gc" foo)
;;

let cTKtoCAMLunits str = 
  let len = String.length str in
  let num_part str = String.sub str 0 (len - 1) in
  match String.get str (pred len) with
    'c' -> Centimeters (float_of_string (num_part str))
  | 'i' -> Inches (float_of_string (num_part str))
  | 'm' -> Millimeters (float_of_string (num_part str))
  | 'p' -> PrinterPoint (float_of_string (num_part str))
  | _ -> Pixels(int_of_string str)
;;

##else

let cCAMLtoTKunits : units -> tkArgs = function
  | `Pix (foo) -> TkToken (string_of_int foo)
  | `Mm (foo)  -> TkToken(Printf.sprintf "%gm" foo)
  | `In (foo)  -> TkToken(Printf.sprintf "%gi" foo)
  | `Pt (foo) -> TkToken(Printf.sprintf "%gp" foo)
  | `Cm (foo) -> TkToken(Printf.sprintf "%gc" foo)
;;

let cTKtoCAMLunits str = 
  let len = String.length str in
  let num_part str = String.sub str ~pos:0 ~len:(len - 1) in
  match String.get str (pred len) with
  | 'c' -> `Cm (float_of_string (num_part str))
  | 'i' -> `In (float_of_string (num_part str))
  | 'm' -> `Mm (float_of_string (num_part str))
  | 'p' -> `Pt (float_of_string (num_part str))
  | _ -> `Pix(int_of_string str)
;;

##endif
