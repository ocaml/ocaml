let f (a,b) (c,d) =  
  a @ b @ c @ d

let _ = 
  f ([],[3.]) ([0],[])     (* either meant [3] or [0.] *)
