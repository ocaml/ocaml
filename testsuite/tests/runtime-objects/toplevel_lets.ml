(* TEST *)

(* Evaluation order for class expressions *)

(* Everything in a class definition is evaluated at object creation time,
   except for any toplevel let-bindings which are lifted away and
   evaluated at class creation time. *)
let () = print_endline "M1:"
module M1 = struct
  let () = print_endline "Before class"
  class c =
    let () = print_endline "Class init" in
    object end
  let () = print_endline "After class"
  let o1 = new c
  let o2 = new c
end

(* PR 13179 *)
let () = print_endline "M2:"
module M2 = struct
  let () = print_endline "Before class"
  class c =
    let open Unit in
    let () = print_endline "Class init" in
    object end
  let () = print_endline "After class"
  let o1 = new c
  let o2 = new c
end

(* Applications: argument evaluated later *)
let () = print_endline "M3:"
module M3 = struct
  class with_param p = object end
  let () = print_endline "Before class"
  class c =
    let () = print_endline "Class init" in
    with_param (print_endline "Class param")
  let () = print_endline "After class"
  let o1 = new c
  let o2 = new c
end

(* Nested bindings are not toplevel *)
(* Not testing for side effects in arguments, as bytecode and native compilers
   produce different evaluation orders *)
let () = print_endline "M4:"
module M4 = struct
  class with_param p = object end
  let () = print_endline "Before class"
  class c =
    (let () = print_endline "Class init" in
     with_param)
      ()
  let () = print_endline "After class"
  let o1 = new c
  let o2 = new c
end

(* Constraints prevent lifting *)
let () = print_endline "M5:"
module M5 = struct
  let () = print_endline "Before class"
  class c =
    (let () = print_endline "Class init" in
     object end : object end)
  let () = print_endline "After class"
  let o1 = new c
  let o2 = new c
end
