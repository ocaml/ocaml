(* TEST
* function_sections
flags = "-S -function-sections"
** arch_arm
*** native
reference = "${test_source_directory}/func_sections.arm.reference"
** arch_arm64
*** native
reference = "${test_source_directory}/func_sections.arm.reference"
** arch_amd64
*** native
reference = "${test_source_directory}/func_sections.reference"
** arch_i386
*** native
reference = "${test_source_directory}/func_sections.reference"
*)

(* We have a separate reference output for ARM because
   it doesn't emit .text after jump tables. *)

(* Test for anonymous functions which result in a mangled symbol *)
let f4 list =
  List.map (fun s -> String.length s) list

let test1 () =
  f4 ["a";"asfda";"afda"]

(* Test for jump tables*)

let g1 s = s^"*"
let g2 s = "*"^s
let g3 s = "*"^s^"*"

let f5 = function
  | 1 -> g1 "a"
  | 2 -> g2 "b"
  | 3 -> g3 "c"
  | 4 -> g1 "d"
  | 5 -> g2 "e"
  | 6 -> g3 "f"
  | _ -> "x"

let test2 () =
  let list =    [f5 5;
                f5 7;
                f5 15;
                f5 26]
  in
  ignore list

let iter = 1_000

let f0 x = x - 7;
[@@inline never]

let f1 x = x + iter
[@@inline never]

let f2 x = f1(x)
[@@inline never]

let f3 x = f2(x)*f0(x)
[@@inline never]

let test3 () =
  f3 iter


let () =
  ignore (test1 ());
  ignore (test2 ());
  ignore (test3 ());
  ()
