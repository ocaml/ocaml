let failures = ref 0;;

let subtyping = false;;
let instanciation = true;;

DYNTEST 3 as int to int for true in failures;;
DYNTEST 3 as int to float for false in failures;;

DYNTEST (3, 2.0) as int * float to int * float for true in failures;;
DYNTEST (3, 2.0) as int * float to int * int for false in failures;;
DYNTEST (3, 2.0) as int * float to float * int for false in failures;;

DYNTEST `A as [`A] to [`A] for true in failures;;
DYNTEST `A as [`A] to [`B] for false in failures;;
DYNTEST `A as [`A] to [`A|`B] for false in failures;;

SUMMARY in failures;;
