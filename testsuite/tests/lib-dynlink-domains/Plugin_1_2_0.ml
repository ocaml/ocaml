let add x = Store.add x
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 402." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.5ccbc179e8522p+0
let () = Store.add "[0_2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 477." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 113." ^ "That's all"
let () = Store.add "[0_2_1]->[]"
let () = Store.add "[0_2_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.008acff442b78p+0
let d0_0_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_0_0.cmo")
let () = Domain.join d0_0_2_1
let () = Store.add "[0_2_1]->[]"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.b48729ff48129p+0
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.4e9f2d93d5f24p+1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.67607e598a62cp-2
