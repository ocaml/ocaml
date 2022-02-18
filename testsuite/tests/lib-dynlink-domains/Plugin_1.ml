let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 263." ^ "That's all"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 506." ^ "That's all"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_0.cmo"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_1.cmo"
let add x = Store.add x
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_2.cmo"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 259." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.36b1b2903c4a3p+1
let () = Store.add "[1]->[]"
let () = Store.add "[1]->[]"
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 46." ^ "That's all"
let () = Dynlink.loadfile @@ Dynlink.adapt_filename "Plugin_1_3.cmo"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.7e04491cfcf48p+1
let wordy = "This" ^ "is" ^ "a" ^ "very" ^ "useful" ^ "code" ^ "fragment: 496." ^ "That's all"
let sqrt2 =
  let rec find c =
    if Float.abs (c *. c -. 2.) < 1e-3 then c
    else find ((c *. c +. 2.) /. (2. *. c))
  in find 0x1.4a4cfb0327f9p+0
