let failures = ref 0;;

let subtyping = false;;
let instantiation = false;;

type a = [`A];;
type ai = [`A of int];;
type aii = [`A of int * int];;
type a_b = [`A | `B];;
type b = [`B];;
type ai_b = [`A of int | `B];;

DYNTEST `A as a to a for true in failures;;
DYNTEST `A as a to ai for false in failures;;
DYNTEST `A as a to a_b for subtyping in failures;;
DYNTEST `A as a to ai_b for false in failures;;
DYNTEST `A as a_b to a_b for true in failures;;
DYNTEST `A as a_b to a for false in failures;;

DYNTEST `B as b to b for true in failures;;
DYNTEST `B as b to ai_b for subtyping in failures;;
DYNTEST `B as a_b to ai_b for false in failures;;
DYNTEST `B as ai_b to a_b for false in failures;;

DYNTEST `A 3 as ai to ai for true in failures;;
DYNTEST `A 3 as ai to ai_b for subtyping in failures;;
DYNTEST `A 3 as ai_b to ai for false in failures;;
DYNTEST `A 3 as ai_b to ai_b for true in failures;;
DYNTEST `A (3, 4) as aii to aii for true in failures;;
DYNTEST `A (3, 4) as aii to ai for false in failures;;
DYNTEST `A 3 as ai to aii for false in failures;;

SUMMARY in failures;;
