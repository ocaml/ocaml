let failures = ref 0;;

let subtyping = false;;
let instanciation = true;;

module M = struct
  type integer = int;;
  type entier = integer;;
end;;
    
DYNTEST 3 as int to int for true in failures;;
DYNTEST 3 as int to float for false in failures;;

DYNTEST 3 as int to M.integer for true in failures;;
DYNTEST 3 as int to M.entier for true in failures;;
DYNTEST 3 as int to M.entier for true in failures;;
DYNTEST 3 as M.integer to int for true in failures;;
DYNTEST 3 as M.integer to M.integer for true in failures;;
DYNTEST 3 as M.integer to M.entier for true in failures;;
DYNTEST 3 as M.entier to int for true in failures;;
DYNTEST 3 as M.entier to M.integer for true in failures;;
DYNTEST 3 as M.entier to M.entier for true in failures;;

DYNTEST (3, 4) as int * int to int * int for true in failures;;
DYNTEST (3, 4) as int * M.integer to int * int for true in failures;;
DYNTEST (3, 4) as int * M.entier to int * int for true in failures;;
DYNTEST (3, 4) as M.integer * int to int * int for true in failures;;
DYNTEST (3, 4) as M.integer * M.integer to int * int for true in failures;;
DYNTEST (3, 4) as M.integer * M.entier to int * int for true in failures;;
DYNTEST (3, 4) as M.entier * int to int * int for true in failures;;
DYNTEST (3, 4) as M.entier * M.integer to int * int for true in failures;;
DYNTEST (3, 4) as M.entier * M.entier to int * int for true in failures;;

DYNTEST (3, 2.0) as int * float to int * float for true in failures;;
DYNTEST (3, 2.0) as int * float to int * int for false in failures;;
DYNTEST (3, 2.0) as int * float to float * int for false in failures;;

DYNTEST `A as [`A] to [`A] for true in failures;;
DYNTEST `A as [`A] to [`B] for false in failures;;
DYNTEST `A as [`A] to [`A|`B] for false in failures;;

SUMMARY in failures;;
