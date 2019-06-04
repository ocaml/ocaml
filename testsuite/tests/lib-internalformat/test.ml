(* TEST
   * expect
*)

let inspect (format : _ format6) =
  let (CamlinternalFormatBasics.Format (fmt, str)) = format in
  (CamlinternalFormat.string_of_fmt fmt, str);;
[%%expect{|
val inspect : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string * string = <fun>
|}];;

inspect "@[foo@]";;
[%%expect{|
- : string * string = ("@[foo@]", "@[foo@]")
|}];;

inspect "@%%";;
[%%expect{|
- : string * string = ("@%%", "@%%")
|}];;

inspect "@<";;
[%%expect{|
- : string * string = ("@<", "@<")
|}];;

inspect "@[<%s>@]";;
[%%expect{|
- : string * string = ("@[<%s>@]", "@[<%s>@]")
|}];;
