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

(* A Tetris game for CamlTk *)
(* written by Jun P. Furuse *)

open Camltk

exception Done

type cell = {mutable color : int; 
             tag : tagOrId * tagOrId * tagOrId}

type falling_block = {
  mutable pattern: int array list;
  mutable bcolor: int;
  mutable x: int;
  mutable y: int;
  mutable d: int;
  mutable alive: bool
}

let stop_a_bit = 300

let colors = [|
  NamedColor "red";
  NamedColor "yellow";

  NamedColor "blue";
  NamedColor "orange";

  NamedColor "magenta";
  NamedColor "green";

  NamedColor "cyan"
|]

let baseurl = "images/"

let backgrounds = 
  List.map (fun s -> baseurl ^ s)
    [ "dojoji.back.gif";
      "Lambda2back.gif";
      "CamlBook.gif";
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
  Array.map btoi dvec

let init fw =
  let scorev = Textvariable.create ()
  and linev = Textvariable.create ()
  and levv = Textvariable.create ()
  and namev = Textvariable.create ()
  in
  let f = Frame.create fw [BorderWidth (Pixels 2)] in
  let c = Canvas.create f [Width (Pixels (block_size * 10));
                           Height (Pixels (block_size * 20));
                           BorderWidth (Pixels cell_border);
                           Relief Sunken;
                           Background Black]
  and r = Frame.create f [] 
  and r' = Frame.create f [] in

  let nl = Label.create r [Text "Next"; Font "variable"] in
  let nc = Canvas.create r [Width (Pixels (block_size * 4));
                           Height (Pixels (block_size * 4));
                           BorderWidth (Pixels cell_border);
                           Relief Sunken;
                           Background Black] in                    
  let scl = Label.create r [Text "Score"; Font "variable"] in
  let sc = Label.create r [TextVariable scorev; Font "variable"] in
  let lnl = Label.create r [Text "Lines"; Font "variable"] in
  let ln = Label.create r [TextVariable linev; Font "variable"] in
  let levl = Label.create r [Text "Level"; Font "variable"] in
  let lev = Label.create r [TextVariable levv; Font "Variable"] in 
  let newg = Button.create r [Text "New Game"; Font "variable"] in
  let exitg = Button.create r [Text "Quit"; Font "variable"] in

  pack [f] [];
  pack [c; r; r'] [Side Side_Left; Fill Fill_Y];
  pack [nl; nc] [Side Side_Top];
  pack [scl; sc; lnl; ln; levl; lev; newg; exitg] [Side Side_Top];

  let cells_src = Array.create 20 (Array.create 10 ()) in
  let cells = Array.map (Array.map (fun () ->
    {tag= 
        (let t1, t2, t3 =
          Canvas.create_rectangle c 
             (Pixels (-block_size - 8)) (Pixels (-block_size - 8))
             (Pixels (-9))          (Pixels (-9)) [],
          Canvas.create_rectangle c 
             (Pixels (-block_size - 10)) (Pixels (-block_size - 10))
             (Pixels (-11))          (Pixels (-11)) [],
          Canvas.create_rectangle c 
             (Pixels (-block_size - 12)) (Pixels (-block_size - 12))
             (Pixels (-13))          (Pixels (-13)) []
        in
          Canvas.raise_top c t1;
          Canvas.raise_top c t2;
          Canvas.lower_bot c t3;
          t1,t2,t3);
     color= 0})) cells_src
  in
  let nexts_src = Array.create 4 (Array.create 4 ()) in
  let nexts = 
   Array.map (Array.map (fun () ->
    {tag= 
       (let t1, t2, t3 =
          Canvas.create_rectangle nc 
             (Pixels (-block_size - 8)) (Pixels (-block_size - 8))
             (Pixels (-9))          (Pixels (-9)) [],
          Canvas.create_rectangle nc 
             (Pixels (-block_size - 10)) (Pixels (-block_size - 10))
             (Pixels (-11))          (Pixels (-11)) [],
          Canvas.create_rectangle nc 
             (Pixels (-block_size - 12)) (Pixels (-block_size - 12))
             (Pixels (-13))          (Pixels (-13)) []
        in
          Canvas.raise_top nc t1;
          Canvas.raise_top nc t2;
          Canvas.lower_bot nc t3;
          t1, t2, t3);
     color= 0})) nexts_src in
  let game_over () = ()
  in
    [f; c; r; nl; nc; scl; sc; levl; lev; lnl; ln], newg, exitg,
      (c, cells), (nc, nexts), scorev, linev, levv, game_over
  
let cell_get (c, cf) x y =
  (Array.get (Array.get cf y) x).color

let cell_set (c, cf) x y col =
  let cur = Array.get (Array.get cf y) x in
  let t1,t2,t3 = cur.tag in 
    if cur.color = col then () 
    else
    if cur.color <> 0 && col = 0 then
      begin
      Canvas.move c t1
                       (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
                       (Pixels (- block_size * (y + 1) -10 - cell_border * 2));
      Canvas.move c t2
                       (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
                       (Pixels (- block_size * (y + 1) -10 - cell_border * 2));
      Canvas.move c t3
                       (Pixels (- block_size * (x + 1) -10 - cell_border * 2))
                       (Pixels (- block_size * (y + 1) -10 - cell_border * 2))
      end
    else
      begin
        Canvas.configure_rectangle c t2
              [FillColor (Array.get colors (col - 1)); 
               Outline (Array.get colors (col - 1))];
        Canvas.configure_rectangle c t1
              [FillColor Black;
               Outline Black];
        Canvas.configure_rectangle c t3
              [FillColor (NamedColor "light gray");
               Outline (NamedColor "light gray")];
        if cur.color = 0 && col <> 0 then
          begin
            Canvas.move c t1
              (Pixels (block_size * (x+1)+10+ cell_border*2))
              (Pixels (block_size * (y+1)+10+ cell_border*2));
            Canvas.move c t2
              (Pixels (block_size * (x+1)+10+ cell_border*2))
              (Pixels (block_size * (y+1)+10+ cell_border*2));
            Canvas.move c t3
              (Pixels (block_size * (x+1)+10+ cell_border*2))
              (Pixels (block_size * (y+1)+10+ cell_border*2))
          end     
      end;
    cur.color <- col

let draw_block field col d x y =
  for iy = 0 to 3 do
    let base = ref 1 in
    let xd = Array.get d iy in
    for ix = 0 to 3 do
      if xd land !base <> 0 then
        begin
          try cell_set field (ix + x) (iy + y) col with _ -> ()  
        end  
      else
        begin
        (* cell_set field (ix + x) (iy + y) 0 *) ()
        end;
      base := !base lsl 1               
    done
  done 

let timer_ref = (ref None : Timer.t option ref)
(* I know, this should be timer ref, but I'm not sure what should be 
   the initial value ... *)

let remove_timer () =
  match !timer_ref with
  | None -> ()
  | Some t -> Timer.remove t (* ; prerr_endline "removed!" *)

let do_after milli f =
  timer_ref := Some (Timer.add milli f)

let copy_block c = 
  { pattern= !c.pattern;
    bcolor= !c.bcolor;
    x= !c.x;
    y= !c.y;
    d= !c.d;
    alive= !c.alive } 

let _ =
  let top = opentk () in
  let lb = Label.create top []
  and fw = Frame.create top []
  in
  let set_message s = Label.configure lb [Text s] in
  pack [lb; fw] [Side Side_Top]; 
  let score = ref 0 in
  let line = ref 0 in
  let level = ref 0 in
  let time = ref 1000 in
  let blocks = List.map (List.map decode_block) blocks in
  let field = Array.create 26 0 in
  let widgets, newg, exitg, cell_field, next_field,
      scorev, linev, levv, game_over = 
        init fw in
  let canvas = fst cell_field in

  let init_field () =
    for i = 0 to 25 do
      field.(i) <- line_empty
    done;
    field.(23) <- line_full;
    for i = 0 to 19 do
      for j = 0 to 9 do
        cell_set cell_field j i 0
      done
    done;
    for i = 0 to 3 do
      for j = 0 to 3 do
        cell_set next_field j i 0
      done
    done 
  in

  let draw_falling_block fb =
    draw_block cell_field fb.bcolor 
      (List.nth fb.pattern fb.d) (fb.x - 3) (fb.y - 3)

  and erase_falling_block fb =
    draw_block cell_field 0 (List.nth fb.pattern fb.d) (fb.x - 3) (fb.y - 3)
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
              cell_set cell_field j (i + fb.y - 3) 0 
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
          cell_set cell_field j (!cur-3) (cell_get cell_field j (!eye-3))
        done;
        decr eye;
        decr cur 
      done
    with Done -> ();
      for i = 3 to !cur do
        field.(i) <- line_empty;
        for j = 0 to 9 do
          cell_set cell_field j (i-3) 0
        done
      done
 in

  let next = ref 42 (* THE ANSWER *)
  and current =
    ref { pattern= [[|0;0;0;0|]]; bcolor=0; x=0; y=0; d=0; alive= false}
  in

  let draw_next () =
    draw_block next_field (!next+1) (List.hd (List.nth blocks !next)) 0 0 

  and erase_next () =
    draw_block next_field 0 (List.hd (List.nth blocks !next)) 0 0 
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
      if sub m then ()
      else
        begin
          m.x <- m.x + 1;
          if sub m then ()
          else
            begin 
              m.x <- m.x - 2;
              ignore (sub m)
            end  
        end
    else ()
  in

  let image_load =
    let i = Canvas.create_image canvas 
            (Pixels (block_size * 5 + block_size / 2)) 
            (Pixels (block_size * 10 + block_size / 2))
                [Anchor Center] in
    Canvas.lower_bot canvas i;
    let img = Imagephoto.create [] in
    fun file ->
      try 
        Imagephoto.configure img [File file];
        Canvas.configure_image canvas i [ImagePhoto img]
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
        (* Future work: We should gain level after an image is put... *)
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
        do_after stop_a_bit loop
      end

  and loop () =
    let m = copy_block current in
    m.y <- m.y + 1;
    if death_check m then
      begin
        !current.alive <- false;
        stone !current;
        do_after stop_a_bit (fun () ->
          let l = clear !current in
            if l > 0 then
              do_after stop_a_bit (fun () ->
                fall_lines ();
                add_score l;
                do_after stop_a_bit newblock)
            else
              newblock ())
      end
    else
      begin
        erase_falling_block !current;
        draw_falling_block m;
        current := m;
        do_after !time loop
      end
  in

  let bind_game w =
    bind w [([], KeyPress)] (BindSet ([Ev_KeySymString],
      fun e -> 
        match e.ev_KeySymString with
        | "h" ->
            let m = copy_block current in
            m.x <- m.x - 1;
            try_to_move m
        | "j" ->
            let m = copy_block current in
            m.d <- m.d + 1;
            if m.d = List.length m.pattern then m.d <- 0;
            try_to_move m
        | "k" ->
            let m = copy_block current in
            m.d <- m.d - 1;
            if m.d < 0 then m.d <- List.length m.pattern - 1;
            try_to_move m
        | "l" ->
            let m = copy_block current in
            m.x <- m.x + 1;
            try_to_move m
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
      ))
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
    do_after !time loop
  in
   bind_game top;
   Button.configure newg [Command game_init];
   Button.configure exitg [Command (fun () -> closeTk (); exit 0)];
   game_init ()

let _ = Printexc.print mainLoop ()
