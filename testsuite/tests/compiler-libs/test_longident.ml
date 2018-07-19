(* TEST
   include ocamlcommon
   * toplevel
*)

module L = Longident

module Examples = struct
  let foo, _M = L.Lident "foo", L.Lident "M"
  let _M_foo = L.Ldot (_M, "foo")
  let _M_N = L.Ldot (_M, "N")
  let _M_N_foo = L.Ldot (_M_N, "foo")
  let _F, _X = L.Lident "F", L.Lident "X"
  let _F_X = L.Lapply (_F, _X)
  let complex = (* M.F(M.N).N.foo *)
    L.Ldot (L.Ldot (L.Lapply(_F, _M_N), "N"), "foo")

  let all = [foo; _M_foo; _M_N; _M_N_foo; _F_X; complex]
  let all_noapp = [foo; _M_foo; _M_N; _M_N_foo]
end

let roundtrip f g x =
  let y = f (g x) in (x = y, x, y)

let roundtrip_opt f g x =
  let y = f (g x) in (Some x = y, x, y)

let section s = print_newline (); print_endline s;;

section "Longident.flatten";;
let flatten_ident = L.flatten (L.Lident "foo");;
let flatten_dot = L.flatten (L.Ldot (L.Lident "M", "foo"));;
let flatten_apply = L.flatten (L.Lapply (L.Lident "F", L.Lident "X"));;

section "Longident.unflatten";;
let unflatten_empty = L.unflatten [];;
let unflatten_sing = L.unflatten ["foo"];;
let unflatten_dot = L.unflatten ["M"; "N"; "foo"];;

let unflatten_flatten =
  Examples.all_noapp |> List.map (roundtrip_opt L.unflatten L.flatten)
;;

section "Longident.last";;
let last_ident = L.last (L.Lident "foo");;
let last_dot = L.last (L.Ldot (L.Lident "M", "foo"));;
let last_apply = L.last (L.Lapply (L.Lident "F", L.Lident "X"));;
let last_dot_apply = L.Ldot (L.Lapply (L.Lident "F", L.Lident "X"), "foo");;

section "Longident.parse";;
let parse_empty = L.parse "";;
let parse_ident = L.parse "foo";;
let parse_dot = L.parse "M.foo";;
let parse_path = L.parse "M.N.foo";;
let parse_complex = L.parse "F(M.N).N.foo";;

section "Longident.to_string";;
let to_string_ident = L.to_string (L.Lident "foo");;
let to_string_dot = L.to_string (L.Ldot (L.Lident "M", "foo"));;
let to_string_apply = L.to_string (L.Lapply (L.Lident "F", L.Lident "X"));;
let to_string_dot_apply =
  L.to_string (L.Ldot (L.Lapply (L.Lident "F", L.Lident "X"), "foo"));;

let parse_to_string =
  Examples.all_noapp |> List.map (roundtrip L.parse L.to_string)
;;
