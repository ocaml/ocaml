let failures = ref 0;;

let subtyping = false;;
let instanciation = true;;

type integer = int;;
type entier = integer;;

type 'a once = 'a;;
type 'a twice = 'a * 'a;;
type 'a thrice = 'a * 'a * 'a;;
type ('a, 'b) pair = 'a * 'b;;
type ('a, 'b) thing = 'a * 'a * 'b;;
type ('a, 'b, 'c) triple = 'a * 'b * 'c;;

DYNTEST 3 as int to int once for true in failures;;
DYNTEST (3, 4) as int * int to int twice for true in failures;;
DYNTEST (3, 4, 5) as int * int * int to int thrice for true in failures;;
DYNTEST 3 as int to integer once for true in failures;;
DYNTEST (3, 4) as int * int to integer twice for true in failures;;
DYNTEST (3, 4, 5) as int * int * int to integer thrice for true in failures;;
DYNTEST 3 as int to entier once for true in failures;;
DYNTEST (3, 4) as int * int to entier twice for true in failures;;
DYNTEST (3, 4, 5) as int * int * int to entier thrice for true in failures;;

DYNTEST 3 as int to float once for false in failures;;
DYNTEST (3, 4) as int * int to float twice for false in failures;;
DYNTEST 3 as int once to float once for false in failures;;
DYNTEST (3, 4) as int twice to float twice for false in failures;;

DYNTEST (3, 4) as entier * int to int twice for true in failures;;
DYNTEST (3, 4) as entier * integer to integer twice for true in failures;;
DYNTEST (3, 4) as int * entier to integer twice for true in failures;;
DYNTEST (3, 4) as int * integer to entier twice for true in failures;;

SUMMARY in failures;;
