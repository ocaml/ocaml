let failures = ref 0;;

let subtyping = false;;
let instanciation = true;;

type integer = int;;
type entier = integer;;

type ('a, 'b) pair = 'a * 'b;;
type ('a, 'b, 'c) triple = 'a * 'b * 'c;;
type 'a twice = ('a, 'a) pair;;
type int2 = int twice;;
type int_int = int * int;;
type integer2 = integer twice;;
type integer_integer = integer * integer;;
type entier2 = entier twice;;
type entier_entier = entier * entier;;

DYNTEST (3, 4) as int2 to int * int for true in failures;;
DYNTEST (3, 4) as int_int to int * int for true in failures;;
DYNTEST (3, 4) as integer2 to int * int for true in failures;;
DYNTEST (3, 4) as integer_integer to int * int for true in failures;;
DYNTEST (3, 4) as entier2 to int * int for true in failures;;
DYNTEST (3, 4) as entier_entier to int * int for true in failures;;

DYNTEST (3, 4) as entier_entier to integer2 for true in failures;;
DYNTEST (3, 4) as integer twice to int * entier for true in failures;;

SUMMARY in failures;;
