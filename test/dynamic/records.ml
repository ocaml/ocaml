let failures = ref 0;;

let subtyping = false;;
let instantiation = false;;

type a = { a : int };; let a = {a=3};;
type b = { b : int };; let b = {b=4};;
type ab = { a : int; b : int };; let ab = {a=3; b=4};;
type ba = { b : int; a : int };; let ba = {a=3; b=4};;
type ma = { mutable a : int };; let ma = {a=3};;
type mab = { mutable a : int; b : int };; let mab = {a=3; b=4};;
type amb = { a : int; mutable b : int };; let amb = {a=3; b=4};;
type mamb = { mutable a : int; mutable b : int };; let mamb = {a=3; b=4};;

DYNTEST a as a to a for true in failures;;
DYNTEST a as a to b for false in failures;;
DYNTEST a as a to ab for false in failures;;
DYNTEST a as a to ba for false in failures;;
DYNTEST a as a to ma for false in failures;;

DYNTEST ab as ab to ab for true in failures;;
DYNTEST ab as ab to ba for false in failures;;
DYNTEST ab as ab to mab for false in failures;;
DYNTEST ab as ab to amb for false in failures;;
DYNTEST ab as ab to mamb for false in failures;;

DYNTEST ma as ma to ma for true in failures;;
DYNTEST ma as ma to a for false in failures;;

DYNTEST mab as mab to mab for true in failures;;
DYNTEST mab as mab to ab for false in failures;;
DYNTEST mab as mab to amb for false in failures;;
DYNTEST mab as mab to mamb for false in failures;;

type ci = {contents : int};;
type cmi = {mutable contents : int};;
type ('a, 'b) c = {contents : 'a * 'b};;
type ('a, 'b) mc = {mutable contents : 'a * 'b};;

DYNTEST ref 3 as int ref to int ref for true in failures;;
DYNTEST ref 3 as int ref to a for false in failures;;
DYNTEST ref 3 as int ref to ma for false in failures;;
DYNTEST ref 3 as int ref to ci for false in failures;;
DYNTEST ref 3 as int ref to cmi for true in failures;;
DYNTEST ref (3, 2.0) as (int * float) ref to (int * float) ref for true in failures;;
DYNTEST ref (3, 2.0) as (int * float) ref to (int, float) mc for true in failures;;
DYNTEST ref (3, 2.0) as (int * float) ref to (float, int) mc for false in failures;;
DYNTEST ref (3, 2.0) as (int * float) ref to (int, float) c for false in failures;;

type 'a clist = { element : 'a; next : 'a clist };;
let rec clist1 = { element = 3; next = clist1 };;
type 'a cn = { element : 'a; next : 'a clist };;
let cn = { element = 4; next = clist1 };;
type 'a clist' = { element : 'a; next : 'a clist' };;
let rec clist'1 = { element = 3; next = clist'1 };;

DYNTEST clist1 as int clist to int clist for true in failures;;
DYNTEST clist1 as int clist to float clist for false in failures;;
DYNTEST cn as int cn to int cn for true in failures;;
DYNTEST cn as int cn to int clist for true in failures;;
DYNTEST clist1 as int clist to int clist' for true in failures;;

SUMMARY in failures;;
