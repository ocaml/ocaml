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


type ('a, 'b, 'c, 'd) change =
  | Delete of 'a
  | Insert of 'b
  | Keep of 'a * 'b * 'c
  | Change of 'a * 'b * 'd

type ('a, 'b, 'c, 'd) patch = ('a, 'b, 'c, 'd) change list

let map f g = function
  | Delete x -> Delete (f x)
  | Insert x -> Insert (g x)
  | Keep (x,y,k) -> Keep (f x, g y, k)
  | Change (x,y,k) -> Change (f x, g y, k)

type ('inner,'line,'column) full_state = {
  line: 'line array;
  column: 'column array;
  inner:'inner
}

module Matrix : sig

  type ('content,'state,'line,'column) t

  val make : lines:int -> columns:int -> ('a,'st,'l,'c) t
  val reshape : lines:int -> columns:int -> ('a,'st,'l,'c) t -> ('a,'st,'l,'c) t

  val diff : ('a,_,_,_) t -> int -> int -> 'a option
  val state : (_,'st,'l,'c) t -> int -> int -> ('st,'l,'c) full_state option
  val weight : _ t -> int -> int -> int

  val line : (_,_,'l,_) t -> int -> int -> 'l option
  val column : (_,_,_,'c) t -> int -> int -> 'c option

  val update :
    ('a,'st,'l,'c) t -> int -> int ->
    ?diff:'a -> weight:int -> state:('st, 'l, 'c) full_state -> unit

  val shape : _ t -> int * int
  val real_shape : _ t -> int * int

end = struct

  type ('content,'state,'line,'column) t =
    { state: ('state,'line,'column) full_state option array array;
      weight: int array array;
      diff: 'content option array array;
      columns: int;
      lines: int;
    }
  let opt_get a n =
    if n < Array.length a then Some (Array.unsafe_get a n) else None
  let line m i j = let* st = m.state.(i).(j) in opt_get st.line i
  let column m i j = let* st = m.state.(i).(j) in opt_get st.column j
  let diff m i j = m.diff.(i).(j)
  let weight m i j = m.weight.(i).(j)
  let state m i j = m.state.(i).(j)
  let shape m = m.lines, m.columns
  
  let update m i j ?diff ~weight ~state =
    m.weight.(i).(j) <- weight;
    m.state.(i).(j) <- Some state;
    m.diff.(i).(j) <- diff;
    ()
  
  let real_shape tbl =
    let lines = ref tbl.lines in
    let columns = ref tbl.columns in
    let max_at i j =
      let*! st = tbl.state.(i).(j) in
      let l = Array.length st.line in
      let c = Array.length st.column in
      if l > !lines then lines := l;
      if c > !columns then columns := c
    in
    for i = 0 to tbl.lines do max_at i tbl.columns done;
    for j = 0 to tbl.columns do max_at tbl.lines j done;
    !lines, !columns

  let make ~lines ~columns =
    { state = Array.make_matrix (lines + 1) (columns + 1) None;
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
    { state = copy None m.state;
      weight = copy max_int m.weight;
      diff = copy None m.diff;
      lines;
      columns
    }

end


let select_best_proposition l =
  let compare_proposition curr prop =
    let* curr_m, curr_res = curr in
    let* m, res = prop in
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
      match test state.inner line column with
      | Ok ok -> Some (Keep (line, column, ok))
      | Error err -> Some (Change (line, column, err))
    in
    compute_proposition (i-1) (j-1) diff
  in
  let*! newweight, (diff, localstate) =
    select_best_proposition [del;insert;diag]
  in
  let newstate = update diff localstate in
  Matrix.update tbl i j ~weight:newweight ~state:newstate ~diff

let compute_matrix ~weight ~test ~update state0 =
  let compute_inner_cell = compute_inner_cell ~test ~update ~weight in
  let m0 = Matrix.make ~lines:0 ~columns:0 in
  Matrix.update m0 0 0 ~weight:0 ~state:state0 ?diff:None;
  let rec loop m =
    let orig_lines, orig_columns = Matrix.shape m in
    let lines, columns = Matrix.real_shape m in
    if lines > orig_lines || columns > orig_columns then
      let m = Matrix.reshape ~lines ~columns m in
      for j = orig_columns + 1 to columns do
        compute_line0 ~update ~weight m j;
        for i = 1 to orig_lines do
          compute_inner_cell m i j
        done
      done;
      for i = orig_lines + 1 to lines do
        compute_column0 ~update ~weight m i;
        for j = 1 to columns do
          compute_inner_cell m i j
        done
      done;
      loop m
    else m in
  loop m0

let construct_diff mat =
  let rec aux acc (i, j) =
    if i = 0 && j = 0 then
      acc
    else
      match Matrix.diff mat i j with
      | None -> acc
      | Some d ->
          let next = match d with
            | Keep _ | Change _ -> (i-1, j-1)
            | Delete _ -> (i-1, j)
            | Insert _ -> (i, j-1)
          in
          aux (d::acc) next
  in
  aux [] (Matrix.shape mat)



let dynamically_resized_diff ~weight ~test ~update state =
  construct_diff (compute_matrix ~weight ~test ~update state)

let diff ~weight ~test ~update state line column =
  let update d fs = { fs with inner = update d fs.inner } in
  let state = { line; column; inner=state } in
  dynamically_resized_diff ~weight ~test ~update state
