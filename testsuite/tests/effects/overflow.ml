(* TEST
 *)

open Effect
open Effect.Deep

type _ t += E : unit t

let f a b c d e f g h =
   let bb = b + b in
   let bbb = bb + b in
   let cc = c + c in
   let ccc = cc + c in
   let dd = d + d in
   let ddd = dd + d in
   let ee = e + e in
   let eee = ee + e in
   let ff = f + f in
   let fff = ff + f in
   let gg = g + g in
   let ggg = gg + g in
   let hh = h + h in
   let hhh = hh + h in
   min 20 a +
     b + bb + bbb +
     c + cc + ccc +
     d + dd + ddd +
     e + ee + eee +
     f + ff + fff +
     g + gg + ggg +
     h + hh + hhh

let () =
  match_with (fun _ -> f 1 2 3 4 5 6 7 8) ()
  { retc = (fun n -> Printf.printf "%d\n" n);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun k -> assert false)
      | _ -> None }
