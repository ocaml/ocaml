(* TEST
*)

[@@@warning "-unerasable-optional-argument"]
let foo ?a =
    print_endline "a parameter";
    fun ~b ->
    print_endline "b parameter";
    fun ~c ->
    print_endline "c parameter"

let f = foo ~a:(print_endline "a argument") ~c:(print_endline "c argument")

let _ = print_endline "f defined"

let _ = f ~b:(print_endline "b argument")


let many ?(arg1=print_endline"arg1") ?(arg2=print_endline"arg2")
  ?(arg3=print_endline"arg3") ?(arg4=print_endline"arg4")
  ?(arg5=print_endline"arg5") ?(arg6=print_endline"arg6")
  ?(arg7=print_endline"arg7") ?(arg8=print_endline"arg8")
  ?(arg9=print_endline"arg9") ?(arg10=print_endline"arg10")
  ?(arg11=print_endline"arg11") ?(arg12=print_endline"arg12")
  ?(arg13=print_endline"arg13") ?(arg14=print_endline"arg14")
  ?(arg15=print_endline"arg15") ?(arg16=print_endline"arg16")
  ?(arg17=print_endline"arg17") ?(arg18=print_endline"arg18")
  ?(arg19=print_endline"arg19") ?(arg20=print_endline"arg20") =
  print_endline "all options"; fun () -> print_endline "all args"

let f = many ~arg10:() ~arg20:()
let g = f ~arg19:()
let () = g ()
