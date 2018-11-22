(* TEST
   reference="${test_source_directory}/ellipses.reference"
   output="ellipses.output"
   script = "${ocamlrun} ${ocamlsrcdir}/tools/caml-tex \
   -repo-root ${ocamlsrcdir} ${test_source_directory}/${test_file} -o ${output}"
  * hasstr
  ** native-compiler
  *** shared-libraries
  **** script with unix,str
  ***** check-program-output
*)

\begin{caml_example*}{verbatim}
let start = 0
[@@@ellipsis.start]
let hidden = succ start
[@@@ellipsis.stop]
let mid = succ hidden
let[@ellipsis] statement = succ mid

module E = struct end
include E[@@ellipsis]

let expr = succ statement[@ellipsis]

let pat = match start with
  | 0[@ellipsis] | 1 -> succ expr
  | _ -> succ expr

let case = match start with
  | 0 -> succ pat
  | _[@ellipsis.start] -> succ pat[@ellipsis.stop]


let annot: int[@ellipsis] = succ case

let subexpr = succ annot + (2[@ellipsis.stop] - 1[@ellipsis.start] * 2) - 2

class[@ellipsis] c = object val x = succ subexpr end

class c2 = object
  val[@ellipsis] x = 0
  val y = 1
  method[@ellipsis] m = 2
  method n = 3
  [@@@ellipsis.start]
  method l = 4
  [@@@ellipsis.stop]
end

type t = A[@ellipsis] | B |C[@ellipsis.start] | D | E [@ellipsis.stop] | F
type arrow = int -> (int -> int[@ellipsis])
type record = { a:int; b:int[@ellipsis]; c:int;
                d:int[@ellipsis.start]; e:int; f:int[@ellipsis.stop];
                g:int }
type polyvar = [`A|`B[@ellipsis] |`C
               |`D[@ellipsis.start] | `E | `F [@ellipsis.stop]
               | `G ]
type exn += A[@ellipsis] | B |C[@ellipsis.start] | D | E [@ellipsis.stop] | F
\end{caml_example*}
