(* TEST
 toplevel;
*)

(* Reminder: quoted strings *)
{é|Some text|é};;
{e|Some more text|e};;

(* Reminder: invalid delimiters for quoted strings *)
{p'|Some text|p'};;
{A|Some other text |A};;
{À|Some other text |À};;


let one = (* strings in comments: "*)" "(*" *) 1;;

let two = (* We are not starting a quoted string here: {p'|, {A|, {À|, {x1|*) 2;;
let three =
  (* We are not starting a quoted string here:
    {p'|(*|p'}|, {A|(*|A}, {À|(*|À}, {x1|(*|x1} *) *) *) *) *)
  3;;


let four = (* We are inserting quoted litteral here {p|*)|p}, {œ|(*|œ} *) 4;;

let set = (** [x < min({x'|x'∊l})] *) 5;;

let unit = let é' = ignore in é' '"';;


let meta = (* (* Reminder: quoted strings *)
{é|Some text|é};;
{e|Some more text|e};;

(* Reminder: invalid delimiters for quoted strings *)
{p'|Some text|p'};;
{A|Some other text |A};;
{À|Some other text |À};;

let one = (* strings in comments: "*)" "(*" *) 1;;

let two = (* We are not starting a quoted string here: {p'|, {A|, {À|, {x1|*) 2;;
let three =
  (* We are not starting a quoted string here neither:
    {p'|(*|p'}|, {A|(*|A}, {À|(*|À}, {x1|(*|x1} *) *) *) *) *)
  3;;


let four = (* We are inserting quoted litteral here {p|*)|p}, {œ|(*|œ} *) 4;;

let set = (** [x < min({x'|x'∊l})] *) 5;;

let unit = let é' = ignore in é' '"';; *) 6;;
