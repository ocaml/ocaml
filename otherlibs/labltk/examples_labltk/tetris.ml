(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* A Tetris game for LablTk *)
(* written by Jun P. Furuse *)

open StdLabels
open Tk

exception Done

type falling_block = {
    mutable pattern: int array list;
    mutable bcolor: int;
    mutable x: int;
    mutable y: int;
    mutable d: int;
    mutable alive: bool
  }

let stop_a_bit = 300

let field_width = 10
let field_height = 20

let colors = [|
  `Color "red";
  `Color "yellow";

  `Color "blue";
  `Color "orange";

  `Color "magenta";
  `Color "green";

  `Color "cyan"
|]

(* Put here your favorite image files *)
let backgrounds = [
  "Lambda2.back.gif"
]

(* blocks *)
let block_size = 16
let cell_border = 2

let blocks = [
  [ [|"0000";
      "0000";
      "1111";
      "0000" |];

    [|"0010";
      "0010";
      "0010";
      "0010" |];

    [|"0000";
      "0000";
      "1111";
      "0000" |];

    [|"0010";
      "0010";
      "0010";
      "0010" |] ];

  [ [|"0000";
      "0110";
      "0110";
      "0000" |];

    [|"0000";
      "0110";
      "0110";
      "0000" |];

    [|"0000";
      "0110";
      "0110";
      "0000" |];

    [|"0000";
      "0110";
      "0110";
      "0000" |] ];

  [ [|"0000";
      "0111";
      "0100";
      "0000" |]; 

    [|"0000";
      "0110";
      "0010";
      "0010" |];

    [|"0000";
      "0010";
      "1110";
      "0000" |];

    [|"0100";
      "0100";
      "0110";
      "0000" |] ];

  [ [|"0000";
      "0100";
      "0111";
      "0000" |]; 

    [|"0000";
      "0110";
      "0100";
      "0100" |];

    [|"0000";
      "1110";
      "0010";
      "0000" |];

    [|"0010";
      "0010";
      "0110";
      "0000" |] ];

  [ [|"0000";
      "1100";
      "0110";
      "0000" |];

    [|"0010";
      "0110";
      "0100";
      "0000" |];

    [|"0000";
      "1100";
      "0110";
      "0000" |];

    [|"0010";
      "0110";
      "0100";
      "0000" |] ];

  [ [|"0000";
      "0011";
      "0110";
      "0000" |];

    [|"0100";
      "0110";
      "0010";
      "0000" |];

    [|"0000";
      "0011";
      "0110";
      "0000" |];

    [|"0000";
      "0100";
      "0110";
      "0010" |] ];

  [ [|"0000";
      "0000";
      "1110";
      "0100" |];

    [|"0000";
      "0100";
      "1100";
      "0100" |];

    [|"0000";
      "0100";
      "1110";
      "0000" |];

    [|"0000";
      "0100";
      "0110";
      "0100" |] ]

]

let line_empty = int_of_string "0b1110000000000111"
let line_full = int_of_string  "0b1111111111111111"

let decode_block dvec =
  let btoi d = int_of_string ("0b"^d) in
  Array.map ~f:btoi dvec

class cell t1 t2 t3 ~canvas ~x ~y = object
  val mutable color = 0
  method get = color
  method set ~color:col =
    if color = col then () else
    if color <> 0 && col = 0 then begin
      Canvas.move canvas t1
        ~x:(- block_size * (x + 1) -10 - cell_border * 2)
        ~y:(- block_size * (y + 1) -10 - cell_border * 2);
      Canvas.move canvas t2
        ~x:(- block_size * (x + 1) -10 - cell_border * 2)
        ~y:(- block_size * (y + 1) -10 - cell_border * 2);
      Canvas.move canvas t3
        ~x:(- block_size * (x + 1) -10 - cell_border * 2)
        ~y:(- block_size * (y + 1) -10 - cell_border * 2)
    end else begin
      Canvas.configure_rectangle canvas t2
        ~fill: colors.(col - 1)
        ~outline: colors.(col - 1);
      Canvas.configure_rectangle canvas t1
        ~fill: `Black
        ~outline: `Black;
      Canvas.configure_rectangle canvas t3
        ~fill: (`Color "light gray")
        ~outline: (`Color "light gray");
      if color = 0 && col <> 0 then begin
        Canvas.move canvas t1
          ~x: (block_size * (x+1)+10+ cell_border*2)
          ~y: (block_size * (y+1)+10+ cell_border*2);
        Canvas.move canvas t2
          ~x: (block_size * (x+1)+10+ cell_border*2)
          ~y: (block_size * (y+1)+10+ cell_border*2);
        Canvas.move canvas t3
          ~x: (block_size * (x+1)+10+ cell_border*2)
          ~y: (block_size * (y+1)+10+ cell_border*2)
      end     
    end;
    color <- col
end
    
let cell_get (c, cf) x y = cf.(y).(x) #get

let cell_set (c, cf) ~x ~y ~color =
  if x >= 0 && y >= 0 && Array.length cf > y && Array.length cf.(y) > x then
    let cur = cf.(y).(x) in
    if cur#get = color then () else cur#set ~color

let create_base_matrix ~cols ~rows =
  let m = Array.create_matrix ~dimx:rows ~dimy:cols (0,0) in
  for x = 0 to cols - 1 do for y = 0 to rows - 1 do
    m.(y).(x) <- (x,y)
  done done;
  m

let init fw =
  let scorev = Textvariable.create ()
  and linev = Textvariable.create ()
  and levv = Textvariable.create ()
  and namev = Textvariable.create ()
  in
  let f = Frame.create fw ~borderwidth: 2 in
  let c = Canvas.create f ~width: (block_size * 10)
                          ~height: (block_size * 20)
                          ~borderwidth: cell_border
                          ~relief: `Sunken
                          ~background: `Black
  and r = Frame.create f
  and r' = Frame.create f in

  let nl = Label.create r ~text: "Next"  ~font: "variable" in
  let nc = Canvas.create r ~width: (block_size * 4)
                           ~height: (block_size * 4)
                           ~borderwidth: cell_border
                           ~relief: `Sunken
                           ~background: `Black in
  let scl = Label.create r ~text: "Score" ~font: "variable" in
  let sc = Label.create r ~textvariable: scorev ~font: "variable" in
  let lnl = Label.create r ~text: "Lines" ~font: "variable" in
  let ln = Label.create r ~textvariable: linev ~font: "variable" in
  let levl = Label.create r ~text: "Level" ~font: "variable" in
  let lev = Label.create r ~textvariable: levv ~font: "variable" in 
  let newg = Button.create r ~text: "New Game" ~font: "variable" in

  pack [f];
  pack [coe c; coe r; coe r'] ~side: `Left ~fill: `Y;
  pack [coe nl; coe nc] ~side: `Top;
  pack [coe scl; coe sc; coe lnl; coe ln; coe levl; coe lev; coe newg]
    ~side: `Top;

  let cells_src = create_base_matrix ~cols:field_width ~rows:field_height in
  let cells =
    Array.map cells_src ~f:
      (Array.map ~f:
         begin fun (x,y) ->
           let t1 =
             Canvas.create_rectangle c 
               ~x1:(-block_size - 8) ~y1:(-block_size - 8)
               ~x2:(-9)              ~y2:(-9)
           and t2 =
             Canvas.create_rectangle c 
               ~x1:(-block_size - 10) ~y1:(-block_size - 10)
               ~x2:(-11)              ~y2:(-11)
           and t3 =
             Canvas.create_rectangle c
               ~x1:(-block_size - 12) ~y1:(-block_size - 12)
               ~x2:(-13)              ~y2:(-13)
           in
           Canvas.raise c t1;
           Canvas.raise c t2;
           Canvas.lower c t3;
           new cell ~canvas:c ~x ~y t1 t2 t3
         end)
  in
  let nexts_src = create_base_matrix ~cols:4 ~rows:4 in
  let nexts =
    Array.map nexts_src ~f:
      (Array.map ~f:
         begin fun (x,y) ->
           let t1 =
             Canvas.create_rectangle nc 
               ~x1:(-block_size - 8) ~y1:(-block_size - 8)
               ~x2:(-9)              ~y2:(-9)
           and t2 =
             Canvas.create_rectangle nc 
               ~x1:(-block_size - 10) ~y1:(-block_size - 10)
               ~x2:(-11)              ~y2:(-11)
           and t3 =
             Canvas.create_rectangle nc 
               ~x1:(-block_size - 12) ~y1:(-block_size - 12)
               ~x2:(-13)              ~y2:(-13)
           in
           Canvas.raise nc t1;
           Canvas.raise nc t2;
           Canvas.lower nc t3;
           new cell ~canvas:nc ~x ~y t1 t2 t3
         end)
  in
  let game_over () = ()
  in
    (* What a mess ! *)
  [ coe f; coe c; coe r; coe nl; coe nc; coe scl; coe sc; coe levl; coe lev; 
    coe lnl; coe ln ],
  newg, (c, cells), (nc, nexts), scorev, linev, levv, game_over
  

let draw_block field ~color ~block ~x ~y =
  for iy = 0 to 3 do
    let base = ref 1 in
    let xd = block.(iy) in
    for ix = 0 to 3 do
      if xd land !base <> 0 then
        cell_set field ~x:(ix + x) ~y:(iy + y) ~color;
      base := !base lsl 1
    done
  done

let timer_ref = (ref None : Timer.t option ref)
(* I know, this should be timer ref, but I'm not sure what should be 
   the initial value ... *)

let remove_timer () =
  match !timer_ref with
    None -> ()
  | Some t -> Timer.remove t (* ; prerr_endline "removed!" *)

let do_after ~ms ~callback =
  timer_ref := Some (Timer.add ~ms ~callback)

let copy_block c = 
  { pattern= !c.pattern;
    bcolor= !c.bcolor;
    x= !c.x;
    y= !c.y;
    d= !c.d;
    alive= !c.alive } 

let _ =
  let top = openTk () in
  let lb = Label.create top
  and fw = Frame.create top
  in
  let set_message s = Label.configure lb ~text:s in
  pack [coe lb; coe fw] ~side: `Top; 
  let score = ref 0 in
  let line = ref 0 in
  let level = ref 0 in
  let time = ref 1000 in
  let blocks = List.map ~f:(List.map ~f:decode_block) blocks in
  let field = Array.create 26 0 in
  let widgets, button, cell_field, next_field, scorev, linev, levv, game_over
      = init fw in
  let canvas = fst cell_field in
  
  let init_field () =
    for i = 0 to 25 do
      field.(i) <- line_empty
    done;
    field.(23) <- line_full;
    for i = 0 to 19 do
      for j = 0 to 9 do
        cell_set cell_field ~x:j ~y:i ~color:0
      done
    done;
    for i = 0 to 3 do
      for j = 0 to 3 do
        cell_set next_field ~x:j ~y:i ~color:0
      done
    done 
  in
  
  let draw_falling_block fb =
    draw_block cell_field ~color: fb.bcolor 
      ~block: (List.nth fb.pattern fb.d) 
      ~x:     (fb.x - 3) 
      ~y:     (fb.y - 3)
    
  and erase_falling_block fb =
    draw_block cell_field ~color: 0 
      ~block: (List.nth fb.pattern fb.d) 
      ~x:     (fb.x - 3) 
      ~y:     (fb.y - 3)
  in

  let stone fb =
    for i=0 to 3 do
      let cur = field.(i + fb.y) in
      field.(i + fb.y) <-
         cur lor ((List.nth fb.pattern fb.d).(i) lsl fb.x)
    done;
    for i=0 to 2 do
      field.(i) <- line_empty
    done

  and clear fb =
    let l = ref 0 in
    for i = 0 to 3 do
      if i + fb.y >= 3 && i + fb.y <= 22 then 
        if field.(i + fb.y) = line_full then
          begin
            incr l;
            field.(i + fb.y) <- line_empty;
            for j = 0 to 9 do
              cell_set cell_field ~x:j ~y:(i + fb.y - 3) ~color:0 
            done
          end  
    done;
    !l
    
  and fall_lines () =
    let eye = ref 22 (* bottom *)
    and cur = ref 22 (* bottom *) 
    in
    try
      while !eye >= 3 do
        while field.(!eye) = line_empty do
          decr eye;
          if !eye = 2 then raise Done
        done;
        field.(!cur) <- field.(!eye);
        for j = 0 to 9 do
          cell_set cell_field ~x:j ~y:(!cur-3) 
            ~color:(cell_get cell_field j (!eye-3))
        done;
        decr eye;
        decr cur 
      done
    with Done -> ();
      for i = 3 to !cur do
        field.(i) <- line_empty;
        for j = 0 to 9 do
          cell_set cell_field ~x:j ~y:(i-3) ~color:0
        done
      done
  in

  let next = ref 42 (* THE ANSWER *)
  and current =
    ref { pattern= [[|0;0;0;0|]]; bcolor=0; x=0; y=0; d=0; alive= false}
  in
     
  let draw_next () =
    draw_block next_field ~color: (!next+1) 
      ~block: (List.hd (List.nth blocks !next)) 
      ~x: 0 ~y: 0 
     
  and erase_next () =
    draw_block next_field ~color: 0 
      ~block: (List.hd (List.nth blocks !next)) 
      ~x: 0 ~y: 0 
  in

  let set_nextblock () =
    current := 
       { pattern= (List.nth blocks !next);
         bcolor= !next+1;
         x=6; y= 1; d= 0; alive= true};
    erase_next ();
    next := Random.int 7;
    draw_next ()
  in
 
  let death_check fb =
    try
      for i=0 to 3 do
        let cur = field.(i + fb.y) in
        if cur land ((List.nth fb.pattern fb.d).(i) lsl fb.x) <> 0 
        then raise Done
      done;
      false
    with 
      Done -> true
  in
    
  let try_to_move m =
    if !current.alive then
      let sub m = 
        if death_check m then false
        else
          begin
            erase_falling_block !current;
            draw_falling_block m;
            current := m;
            true
          end
      in
      if sub m then true
      else        
        begin
          m.x <- m.x + 1;
          if sub m then true
          else
            begin 
              m.x <- m.x - 2;
              sub m
            end  
        end
    else false 
  in

  let image_load =
    let i = Canvas.create_image canvas 
        ~x: (block_size * 5 + block_size / 2)
        ~y: (block_size * 10 + block_size / 2)
        ~anchor: `Center in
    Canvas.lower canvas i;
    let img = Imagephoto.create () in
    fun file ->
      try 
        Imagephoto.configure img ~file: file;
        Canvas.configure_image canvas i ~image: img 
      with
        _ -> 
          begin
            Printf.eprintf "%s : No such image...\n" file;
            flush stderr
          end
  in

  let add_score l =
    let pline = !line in
    if l <> 0 then
      begin
        line := !line + l; 
        score := !score + l * l;
        set_message (Printf.sprintf "%d pts" (1 lsl ((l - 1) * 2)))
      end; 
    Textvariable.set linev (string_of_int !line);
    Textvariable.set scorev (string_of_int !score); 

    if !line /10 <> pline /10 then 
      (* update the background every 10 lines. *)
      begin
        let num_image = List.length backgrounds - 1 in
        let n = !line/10 in
        let n = if n > num_image then num_image else n in
        let file = List.nth backgrounds n in
        image_load file;
        incr level; 
        Textvariable.set levv (string_of_int !level) 
      end
  in

  let rec newblock () = 
    set_message "TETRIS";
    set_nextblock ();
    draw_falling_block !current;
    if death_check !current then 
      begin
        !current.alive <- false;
        set_message "GAME OVER";
        game_over ()
      end
    else
      begin
        time := 1100 - (!level / 4 * 300) - ((!level mod 4) * 200);
        if !time < 60 - !level * 3 then time := 60 - !level * 3;
        do_after ~ms:stop_a_bit ~callback:loop
      end
   
  and loop () =
    let m = copy_block current in
    m.y <- m.y + 1;
    if death_check m then
      begin
        !current.alive <- false;
        stone !current;
        do_after ~ms:stop_a_bit ~callback:
          begin fun () ->
            let l = clear !current in
            if l > 0 then
              do_after ~ms:stop_a_bit ~callback:
                begin fun () ->
                  fall_lines ();
                  add_score l;
                  do_after ~ms:stop_a_bit ~callback:newblock
                end
            else
              newblock ()
          end
      end
    else
      begin
        erase_falling_block !current;
        draw_falling_block m;
        current := m;
        do_after ~ms:!time ~callback:loop
      end
  in

  let bind_game w =
    bind w ~events:[`KeyPress] ~fields:[`KeySymString] ~action:
      begin fun e -> 
        match e.ev_KeySymString with
        | "h"|"Left" ->
            let m = copy_block current in
            m.x <- m.x - 1;
            ignore (try_to_move m)
        | "j"|"Up" ->
            let m = copy_block current in
            m.d <- m.d + 1;
            if m.d = List.length m.pattern then m.d <- 0;
            ignore (try_to_move m)
        | "k"|"Down" ->
            let m = copy_block current in
            m.d <- m.d - 1;
            if m.d < 0 then m.d <- List.length m.pattern - 1;
            ignore (try_to_move m)
        | "l"|"Right" ->
            let m = copy_block current in
            m.x <- m.x + 1;
            ignore (try_to_move m)
        | "m" ->
            remove_timer ();
            loop ()
        | "space" ->
            if !current.alive then
              begin
                let m = copy_block current
                and n = copy_block current in
                while 
                  m.y <- m.y + 1;
                  if death_check m then false
                  else begin n.y <- m.y; true end
                do () done;
                erase_falling_block !current;
                draw_falling_block n;
                current := n;
                remove_timer ();
                loop ()
              end  
        | _ -> ()          
      end
  in

  let game_init () =
    (* Game Initialization *)
    set_message "Initializing ...";
    remove_timer ();
    image_load (List.hd backgrounds);
    time := 1000;
    score := 0;
    line := 0;
    level := 1;
    add_score 0; 
    init_field ();
    next := Random.int 7;       
    set_message "Welcome to TETRIS";
    set_nextblock ();
    draw_falling_block !current;
    do_after ~ms:!time ~callback:loop
  in
    (* As an applet, it was required... *)
    (* List.iter f: bind_game widgets; *)
  bind_game top; 
  Button.configure button ~command: game_init;
  game_init ()

let _ = Printexc.print mainLoop ()
