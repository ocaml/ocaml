open Pcre
open Regexp

(* same as Regexp.t *)
type 'a t = {
    string : string;
    typ : typ;
    result : typ -> string array -> 'a
  }

let (>>) (t : 'a Regexp.t) f = fun s -> 
  let t = (Obj.magic t : 'a t) in
  f (t.result t.typ (extract ~pat: t.string s))

let test s t f = (t >> f) s

let rec pmatch (s : string) = function
  | [] -> raise Not_found
  | c::cs -> try c s with Not_found -> pmatch s cs

(* test *)

let _ = 
  pmatch "hoge" [ [/hoge/] >> (fun o -> "hoge");
		  [/ha(g)e/] >> (fun o -> o#_1) ]

let subst s (t : 'a Regexp.t) f =
  let t = (Obj.magic t : 'a t) in
  substitute ~pat: t.string ~subst: f s

(* callout *)

(* type of our callout:
   <...> -> unit
*)
