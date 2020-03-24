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

type ('inner,'line,'col) full_state =
  {
    line: 'line array;
    col: 'col array;
    inner:'inner
  }

module Matrix = struct

  type ('state,'content,'line,'col) t =
    { state: ('state,'line,'col) full_state option array array;
      weight:int array array;
      patch: 'content option array array;
      cols:int;
      lines:int;
    }
  let opt_get a n =
    if n < Array.length a then Some (Array.unsafe_get a n) else None
  let line m i j = let* st = m.state.(i).(j) in opt_get st.line i
  let col m i j = let* st = m.state.(i).(j) in opt_get st.col j
  let patch m i j = m.patch.(i).(j)
  let shape st = Array.length st.line, Array.length st.col

  let real_shape tbl =
    let lines = ref tbl.lines in
    let cols = ref tbl.cols in
    let max_at i j =
      let*! st = tbl.state.(i).(j) in
      let l, c = shape st in
      if l > !lines then lines := l;
      if c > !cols then cols := c
    in
    for i = 0 to tbl.lines do max_at i tbl.cols done;
    for j = 0 to tbl.cols do max_at tbl.lines j done;
    !lines, !cols

  let make ~size =
    let l1, l2 = size in
    { state = Array.make_matrix (l1 + 1) (l2 + 1) None;
      weight = Array.make_matrix (l1 + 1) (l2 + 1) max_int;
      patch = Array.make_matrix (l1 + 1) (l2 + 1) None;
      lines=l1;
      cols=l2;
    }

  let reshape m (lines,cols) =
    let copy default a =
      Array.init (1+lines) (fun i -> Array.init (1+cols) (fun j ->
          if i <= m.lines && j <= m.cols then
            a.(i).(j)
          else default) ) in
    { state = copy None m.state;
      weight = copy max_int m.weight;
      patch = copy None m.patch;
      lines;
      cols
    }

end


let select_best_proposition l =
  let rec aux m0 res0 = function
    | [] -> m0, res0
    | (m', res') :: t ->
        let m'', res'' = if m0 <= m' then m0,res0 else m',res' in
        aux m'' res'' t
  in
  match List.filter_map Fun.id l with
  | [] -> None
  | (m,res)::l ->Some(aux m res l)

let update_cell m i j ?p ~x ~s =
  m.Matrix.weight.(i).(j) <- x;
  m.Matrix.state.(i).(j) <- Some s;
  m.Matrix.patch.(i).(j) <- p;
  ()

let compute_col0 ~update ~weight tbl i =
  let*! st = tbl.Matrix.state.(i-1).(0) in
  let*! line = Matrix.line tbl (i-1) 0 in
  let diff = Delete line in
  update_cell tbl i 0
    ~x:(weight diff + tbl.Matrix.weight.(i-1).(0))
    ~s:(update diff st)
    ~p:diff

let compute_line0 ~update ~weight tbl j =
  let*! st = tbl.Matrix.state.(0).(j-1) in
  let*! col = Matrix.col tbl 0 (j-1) in
  let diff = Insert col  in
  update_cell tbl 0 j
    ~x:(weight diff + tbl.Matrix.weight.(0).(j-1))
    ~s:(update diff st)
    ~p:diff

let compute_inner_cell ~test ~update ~weight tbl i j =
  let open Matrix in
  let compute_proposition i j diff =
    let* diff = diff in
    let+ state = tbl.state.(i).(j) in
    weight diff + tbl.Matrix.weight.(i).(j), (diff, update diff state)
  in
  let del =
    let diff = let+ x = line tbl (i-1) j in Delete x in
    compute_proposition (i-1) j diff
  in
  let insert =
    let diff = let+ x = col tbl i (j-1) in Insert x in
    compute_proposition i (j-1) diff
  in
  let diag =
    let diff =
      let* state = tbl.state.(i-1).(j-1) in
      let* line = line tbl (i-1) (j-1) in
      let* col = col tbl (i-1) (j-1) in
      match test state.inner line col with
      | Ok ok -> Some (Keep (line, col, ok))
      | Error err -> Some (Change (line, col, err))
    in
    compute_proposition (i-1) (j-1) diff
  in
  let*! newweight, (newres, newstate) =
    select_best_proposition [del;insert;diag]
  in
  update_cell tbl i j ~x:newweight ~s:newstate ~p:newres

let compute_matrix ~weight ~test ~update state0 =
  let open Matrix in
  let compute_inner_cell = compute_inner_cell ~test ~update ~weight in
  let m0 = Matrix.make ~size:(0,0) in
  update_cell m0 0 0 ~x:0 ~s:state0 ?p:None;
  let rec loop m =
    let orig_lines = m.lines and orig_cols = m.cols in
    let ext_lines, ext_cols as ext_shape = Matrix.real_shape m in
    if ext_lines > orig_lines || ext_cols > orig_cols then
      let m = Matrix.reshape m ext_shape in
      for j = orig_cols + 1 to ext_cols do
        compute_line0 ~update ~weight m j;
        for i = 1 to orig_lines do
          compute_inner_cell m i j
        done
      done;
      for i = orig_lines + 1 to ext_lines do
        compute_col0 ~update ~weight m i;
        for j = 1 to ext_cols do
          compute_inner_cell m i j
        done
      done;
      loop m
    else m in
  loop m0

let construct_patch mat =
  let rec aux acc (i, j) =
    if i = 0 && j = 0 then
      acc
    else
      match Matrix.patch mat i j with
      | None -> acc
      | Some d ->
          let next = match d with
            | Keep _ | Change _ -> (i-1, j-1)
            | Delete _ -> (i-1, j)
            | Insert _ -> (i, j-1)
          in
          aux (d::acc) next
  in
  aux [] (mat.Matrix.lines, mat.Matrix.cols)



let dynamically_resized_diff ~weight ~test ~update state =
  construct_patch (compute_matrix ~weight ~test ~update state)

let diff ~weight ~test ~update state line col =
  let update d fs = { fs with inner = update d fs.inner } in
  let state = { line; col; inner=state } in
  dynamically_resized_diff ~weight ~test ~update state
