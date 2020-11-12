(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "-16"]

let (let*) = Option.bind
let (let+) x f = Option.map f x
let (let*!) x f = Option.iter f x

type ('left, 'right, 'eq, 'diff) change =
  | Delete of 'left
  | Insert of 'right
  | Keep of 'left * 'right * 'eq
  | Change of 'left * 'right * 'diff

type ('l, 'r, 'eq, 'diff) patch = ('l, 'r, 'eq, 'diff) change list

let map f g = function
  | Delete x -> Delete (f x)
  | Insert x -> Insert (g x)
  | Keep (x,y,k) -> Keep (f x, g y, k)
  | Change (x,y,k) -> Change (f x, g y, k)

type ('st,'left,'right) full_state = {
  line: 'left array;
  column: 'right array;
  state: 'st
}

module Matrix : sig

  type ('state,'left,'right,'eq,'diff) t

  val make : lines:int -> columns:int -> ('st,'l,'r,'e,'d) t
  val reshape :
    lines:int -> columns:int ->
    ('st,'l,'r,'e,'d) t -> ('st,'l,'r,'e,'d) t

  val diff : (_,'l,'r,'e,'d) t -> int -> int -> ('l,'r,'e,'d) change option
  val state : ('st,'l,'r,'e,'d) t -> int -> int -> ('st, 'l, 'r) full_state option
  val weight : _ t -> int -> int -> int

  val line : (_,'l,_,_,_) t -> int -> int -> 'l option
  val column : (_,_,'r,_,_) t -> int -> int -> 'r option

  val update :
    ('st,'l,'r,'e,'d) t -> int -> int ->
    ?diff:('l,'r,'e,'d) change ->
    weight:int ->
    state:('st, 'l, 'r) full_state ->
    unit

  val shape : _ t -> int * int
  val[@warning "-32"] shape_at : _ t -> int -> int -> (int * int) option
  val real_shape : _ t -> int * int

  val[@warning "-32"] pp : Format.formatter -> _ t -> unit
  
end = struct

  type ('state,'left,'right,'eq,'diff) t =
    { states: ('state,'left,'right) full_state option array array;
      weight: int array array;
      diff: ('left,'right,'eq,'diff) change option array array;
      columns: int;
      lines: int;
    }
  let opt_get a n =
    if n < Array.length a then Some (Array.unsafe_get a n) else None
  let line m i j = let* st = m.states.(i).(j) in opt_get st.line i
  let column m i j = let* st = m.states.(i).(j) in opt_get st.column j
  let diff m i j = m.diff.(i).(j)
  let weight m i j = m.weight.(i).(j)
  let state m i j = m.states.(i).(j)
  let shape m = m.lines, m.columns
  
  let update m i j ?diff ~weight ~state =
    m.weight.(i).(j) <- weight;
    m.states.(i).(j) <- Some state;
    m.diff.(i).(j) <- diff;
    ()

  let shape_at tbl i j =
    let+ st = tbl.states.(i).(j) in
    let l = Array.length st.line in
    let c = Array.length st.column in
    l, c
  
  let real_shape tbl =
    let lines = ref tbl.lines in
    let columns = ref tbl.columns in
    let max_at i j =
      let*! l, c = shape_at tbl i j in
      if l > !lines then lines := l;
      if c > !columns then columns := c
    in
    for i = 0 to tbl.lines do 
      for j = 0 to tbl.columns do
        max_at i j
      done;
    done;
    !lines, !columns

  let make ~lines ~columns =
    { states = Array.make_matrix (lines + 1) (columns + 1) None;
      weight = Array.make_matrix (lines + 1) (columns + 1) max_int;
      diff = Array.make_matrix (lines + 1) (columns + 1) None;
      lines;
      columns;
    }

  let reshape ~lines ~columns m =
    let copy default a =
      Array.init (1+lines) (fun i -> Array.init (1+columns) (fun j ->
          if i <= m.lines && j <= m.columns then
            a.(i).(j)
          else default) ) in
    { states = copy None m.states;
      weight = copy max_int m.weight;
      diff = copy None m.diff;
      lines;
      columns
    }


  let pp ppf m =
    let l,c = shape m in
    Format.eprintf "Shape : %i, %i@." l c;
    for i = 0 to l do
      for j = 0 to c do
        let d = diff m i j in
        match d with
        | None ->
            Format.fprintf ppf "    "
        | Some diff ->
            let sdiff = match diff with
              | Insert _ -> "←"
              | Delete _ -> "↑"
              | Keep _ -> "↖"
              | Change _ -> "⇱"
            in
            let w = weight m i j in
            Format.fprintf ppf "%s%i " sdiff w
      done;
      Format.pp_print_newline ppf ()
    done
 
end


let select_best_proposition l =
  let compare_proposition curr prop =
    match curr, prop with
    | None, o | o, None -> o
    | Some (curr_m, curr_res), Some (m, res) ->
        Some (if curr_m <= m then curr_m, curr_res else m,res)
  in
  List.fold_left compare_proposition None l

let compute_column0 ~weight ~update tbl i =
  let*! st = Matrix.state tbl (i-1) 0 in
  let*! line = Matrix.line tbl (i-1) 0 in
  let diff = Delete line in
  Matrix.update tbl i 0
    ~weight:(weight diff + Matrix.weight tbl (i-1) 0)
    ~state:(update diff st)
    ~diff

let compute_line0 ~weight ~update tbl j =
  let*! st = Matrix.state tbl 0 (j-1) in
  let*! column = Matrix.column tbl 0 (j-1) in
  let diff = Insert column  in
  Matrix.update tbl 0 j
    ~weight:(weight diff + Matrix.weight tbl 0 (j-1))
    ~state:(update diff st)
    ~diff

let compute_inner_cell ~weight ~test ~update tbl i j =
  let compute_proposition i j diff =
    let* diff = diff in
    let+ localstate = Matrix.state tbl i j in
    weight diff + Matrix.weight tbl i j, (diff, localstate)
  in
  let del =
    let diff = let+ x = Matrix.line tbl (i-1) j in Delete x in
    compute_proposition (i-1) j diff
  in
  let insert =
    let diff = let+ x = Matrix.column tbl i (j-1) in Insert x in
    compute_proposition i (j-1) diff
  in
  let diag =
    let diff =
      let* state = Matrix.state tbl (i-1) (j-1) in
      let* line = Matrix.line tbl (i-1) (j-1) in
      let* column = Matrix.column tbl (i-1) (j-1) in
      match test state.state line column with
      | Ok ok -> Some (Keep (line, column, ok))
      | Error err -> Some (Change (line, column, err))
    in
    compute_proposition (i-1) (j-1) diff
  in
  let*! newweight, (diff, localstate) =
    select_best_proposition [del;insert;diag]
  in
  let state = update diff localstate in
  Matrix.update tbl i j ~weight:newweight ~state ~diff

let compute_cell ~weight ~test ~update m i j =
  match i, j with
  | _ when Matrix.diff m i j <> None -> ()
  | 0,0 -> ()
  | 0,j -> compute_line0 ~update ~weight m j
  | i,0 -> compute_column0 ~update ~weight m i;
  | _ -> compute_inner_cell ~weight ~test ~update m i j

let compute_matrix ~weight ~test ~update state0 =
  let m0 = Matrix.make ~lines:0 ~columns:0 in
  Matrix.update m0 0 0 ~weight:0 ~state:state0 ?diff:None;
  let rec loop m =
    let orig_lines, orig_columns = Matrix.shape m in
    let lines, columns = Matrix.real_shape m in
    if lines > orig_lines || columns > orig_columns then
      let m = Matrix.reshape ~lines ~columns m in
      for j = 0 to columns do
        for i = 0 to lines do
          compute_cell ~update ~test ~weight m i j
        done
      done;
      loop m
    else m in
  loop m0

let select_final_state m0 =
  let final = ref (0,0, max_int) in
  let l0, c0 = Matrix.shape m0 in
  for i = 0 to l0 do
    for j = 0 to c0 do
      let*! l, c = Matrix.shape_at m0 i j in
      let w = Matrix.weight m0 i j in
      let _, _, w0 = !final in
      if i = l && j = c && w < w0 then begin
        final := (i, j, w)
      end
    done;
  done;
  let i_final, j_final, _ = !final in
  assert (i_final <> 0 || j_final <> 0);
  (i_final, j_final)

let construct_diff m0 =
  let rec aux acc (i, j) =
    if i = 0 && j = 0 then
      acc
    else
      match Matrix.diff m0 i j with
      | None -> assert false
      | Some d ->
          let next = match d with
            | Keep _ | Change _ -> (i-1, j-1)
            | Delete _ -> (i-1, j)
            | Insert _ -> (i, j-1)
          in
          aux (d::acc) next
  in
  aux [] (select_final_state m0)

let diff ~weight ~test ~update state line column =
  let update d fs = { fs with state = update d fs.state } in
  let fullstate = { line; column; state } in
  construct_diff (compute_matrix ~weight ~test ~update fullstate)

type ('l, 'r, 'e, 'd, 'state) update =
  | No of (('l,'r,'e,'d) change -> 'state -> 'state)
  | Left of (('l,'r,'e,'d) change -> 'state -> 'state * 'l array)
  | Right of (('l,'r,'e,'d) change -> 'state -> 'state * 'r array)

let variadic_diff ~weight ~test ~(update:_ update) state line column =
  let update = match update with
    | No up ->
        fun d fs ->
          let state = up d fs.state in
          { fs with state }
    | Left up ->
        fun d fs ->
          let state, a = up d fs.state in
          { fs with state ; line = Array.append fs.line a }
    | Right up -> 
        fun d fs ->
          let state, a = up d fs.state in
          { fs with state ; column = Array.append fs.column a }
  in
  let fullstate = { line; column; state } in
  construct_diff (compute_matrix ~weight ~test ~update fullstate)
