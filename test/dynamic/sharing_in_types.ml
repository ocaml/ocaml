let failures = ref 0;;

let subtyping = false;;
let instanciation = true;;

type integer = int;;
type entier = integer;;

let twice x = x, x;;
let (three : integer) = 3;;
let (quatre : entier) = 4;;

DYNTEST twice three as 'a to integer * entier for true in failures;;
DYNTEST twice three as 'a to entier * integer for true in failures;;
DYNTEST twice quatre as 'a to integer * entier for true in failures;;
DYNTEST twice quatre as 'a to entier * integer for true in failures;;

DYNTEST twice three as 'a to float * float for false in failures;;
DYNTEST twice quatre as 'a to float * float for false in failures;;

DYNTEST twice (twice three) as 'a to (int * integer) * (entier * int) for true in failures;;

SUMMARY in failures;;
