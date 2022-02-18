let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_3_2_0.sqrt2
let () = Store.add "[2_2_3]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_3_2_0.sqrt2
let add x = Plugin_3_0.add x
let () = Plugin_3_2_0_0.add "[2_2_3]->[0_0_2_3]"
let d0_2_2_3 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_3_2_2_0.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 51." ^ Plugin_3_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_3_2_0.sqrt2
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 211." ^ Plugin_3_2_0.wordy
let () = Domain.join d0_2_2_3
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 463." ^ Plugin_3_2_0.wordy
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_3_2_0.sqrt2
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find Plugin_3_2_0.sqrt2
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_3_2_2_1.cmo"
