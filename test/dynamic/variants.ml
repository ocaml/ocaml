let failures = ref 0;;

let subtyping = false;;
let instantiation = true;;

type a = A;;
type b = B | C;;
type a1 = A1 of int;;
type a2 = A2 of int * int;;
type a3 = A3 of (int * int);;

DYNTEST A as a to a for true in failures;;
DYNTEST A as a to b for false in failures;;
DYNTEST B as b to b for true in failures;;
DYNTEST C as b to b for true in failures;;
DYNTEST B as b to a for false in failures;;
DYNTEST C as b to a for false in failures;;

DYNTEST A as a to a1 for false in failures;;
DYNTEST A1 42 as a1 to a for false in failures;;
DYNTEST A1 42 as a1 to a1 for true in failures;;
DYNTEST A1 42 as a1 to a2 for false in failures;;
DYNTEST A1 42 as a1 to a3 for false in failures;;
DYNTEST A2 (42, 43) as a2 to a1 for false in failures;;
DYNTEST A2 (42, 43) as a2 to a2 for true in failures;;
DYNTEST A2 (42, 43) as a2 to a3 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a1 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a2 for false in failures;;
DYNTEST A3 ((42, 43)) as a3 to a3 for true in failures;;

SUMMARY in failures;;
