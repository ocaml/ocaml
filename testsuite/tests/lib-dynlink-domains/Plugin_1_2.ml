let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.367a013a41025p+2
let () = Store.add "[2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 34." ^ "That's all"
let () = Store.add "[2_1]->[]"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_0.cmo"
let d1_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_1.cmo")
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 871." ^ "That's all"
let () = Store.add "[2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 670." ^ "That's all"
let d2_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_2.cmo")
let () = Domain.join d2_2_1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.92916b7632643p-2
let d3_2_1 = Domain.spawn (fun () -> Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2_3.cmo")
let () = Domain.join d3_2_1
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.0de037f4da61p+1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 690." ^ "That's all"
let () = Domain.join d1_2_1
let add x = Store.add x
let () = Store.add "[2_1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 264." ^ "That's all"
