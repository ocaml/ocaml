open Tk

let _ =
 let top = openTk () in
 let fw = Frame.create parent: top () in
   pack [fw];
   let c = Canvas.create parent: fw width: (`Pix 200) height: (`Pix 200) () in
   let create_eye cx cy wx wy ewx ewy bnd =
     let o2 = Canvas.create_oval c 
       x1:(`Pix (cx - wx)) y1:(`Pix (cy - wy)) 
       x2:(`Pix (cx + wx)) y2:(`Pix (cy + wy)) 
         outline: (`Color "black") width: (`Pix 7) 
      	 fill: (`Color "white") 
     and o = Canvas.create_oval c 
       x1:(`Pix (cx - ewx)) y1:(`Pix (cy - ewy)) 
       x2:(`Pix (cx + ewx)) y2:(`Pix (cy + ewy)) 
      	   fill: (`Color "black") in
     let curx = ref cx
     and cury = ref cy in
       bind c events:[[], `Motion]
         action: (`Extend ([`MouseX; `MouseY], (fun e ->
       	   let nx, ny =
      	     let xdiff = e.ev_MouseX - cx 
	     and ydiff = e.ev_MouseY - cy in
	     let diff = sqrt (((float xdiff) /. ((float wx) *. bnd)) ** 2.0 +. 
               ((float ydiff) /. ((float wy) *. bnd)) ** 2.0) in
	     if diff > 1.0 then
      	       truncate ((float xdiff) *. (1.0 /. diff)) + cx,
	       truncate ((float ydiff) *. (1.0 /. diff)) + cy
	     else 
               e.ev_MouseX, e.ev_MouseY
	 in
	   Canvas.move c tag: o 
	     x: (`Pix (nx - !curx)) y: (`Pix (ny - !cury));
	   curx := nx;
	   cury := ny)))
  in
     create_eye 60 100 30 40 5 6 0.6;
     create_eye 140 100 30 40 5 6 0.6;
     pack [c] 

let _ = Printexc.print mainLoop ()

