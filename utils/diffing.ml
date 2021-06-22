(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, Inria Paris               *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@warning "-16"]

(* This module implements a modified version of Wagner-Fischer
   See <https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm>
   for preliminary reading.

   The main extensions is that:
   - State is computed based on the optimal patch so far.
   - The lists can be extended at each state computation.

   We add the constraint that extensions can only be in one side
   (either the left or right list). This is enforced by the external API.

*)

(** Shared types *)
type change_kind =
  | Deletion
  | Insertion
  | Modification
  | Preservation

let style = function
  | Preservation -> Misc.Color.[ FG Green ]
  | Deletion -> Misc.Color.[ FG Red; Bold]
  | Insertion -> Misc.Color.[ FG Red; Bold]
  | Modification -> Misc.Color.[ FG Magenta; Bold]

let prefix ppf (pos, p) =
  let sty = style p in
  Format.pp_open_stag ppf (Misc.Color.Style sty);
  Format.fprintf ppf "%i. " pos;
  Format.pp_close_stag ppf ()


let (let*) = Option.bind
let (let+) x f = Option.map f x
let (let*!) x f = Option.iter f x

module type Defs = sig
  type left
  type right
  type eq
  type diff
  type state
end

type ('left,'right,'eq,'diff) change =
  | Delete of 'left
  | Insert of 'right
  | Keep of 'left * 'right *' eq
  | Change of 'left * 'right * 'diff

let classify = function
    | Delete _ -> Deletion
    | Insert _ -> Insertion
    | Change _ -> Modification
    | Keep _ -> Preservation

module Define(D:Defs) = struct
  open D

type nonrec change = (left,right,eq,diff) change

type patch = change list
module type S = sig
  val diff: state -> left array -> right array -> patch
end


type full_state = {
  line: left array;
  column: right array;
  state: state
}

(* The matrix supporting our dynamic programming implementation.

   Each cell contains:
   - The diff and its weight
   - The state computed so far
   - The lists, potentially extended locally.

   The matrix can also be reshaped.
*)
module Matrix : sig

  type shape = { l : int ; c : int }

  type  t

  val make : shape ->  t
  val reshape : shape ->  t ->  t

  (** accessor functions *)
  val diff : t -> int -> int ->  change option
  val state : t -> int -> int -> full_state option
  val weight : t -> int -> int -> int

  val line : t -> int -> int -> left option
  val column : t -> int -> int -> right option

  val set :
    t -> int -> int ->
    diff:change option ->
    weight:int ->
    state:full_state ->
    unit

  (** the shape when starting filling the matrix *)
  val shape : t -> shape

  (** [shape m i j] is the shape as seen from the state at position (i,j)
      after some possible extensions
  *)
  val shape_at : t -> int -> int -> shape option

  (** the maximal shape on the whole matrix *)
  val real_shape : t -> shape

  (** debugging printer *)
  val[@warning "-32"] pp : Format.formatter -> t -> unit

end = struct

  type shape = { l : int ; c : int }

  type  t =
    { states: full_state option array array;
      weight: int array array;
      diff:  change option array array;
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
  let shape m = { l = m.lines ; c = m.columns }

  let set m i j ~diff ~weight ~state =
    m.weight.(i).(j) <- weight;
    m.states.(i).(j) <- Some state;
    m.diff.(i).(j) <- diff;
    ()

  let shape_at tbl i j =
    let+ st = tbl.states.(i).(j) in
    let l = Array.length st.line in
    let c = Array.length st.column in
    { l ; c }

  let real_shape tbl =
    let lines = ref tbl.lines in
    let columns = ref tbl.columns in
    for i = 0 to tbl.lines do
      for j = 0 to tbl.columns do
        let*! {l; c} = shape_at tbl i j in
        if l > !lines then lines := l;
        if c > !columns then columns := c
      done;
    done;
    { l = !lines ; c = !columns }

  let make { l = lines ; c = columns } =
    { states = Array.make_matrix (lines + 1) (columns + 1) None;
      weight = Array.make_matrix (lines + 1) (columns + 1) max_int;
      diff = Array.make_matrix (lines + 1) (columns + 1) None;
      lines;
      columns;
    }

  let reshape { l = lines ; c = columns } m =
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
    let { l ; c } = shape m in
    Format.eprintf "Shape : %i, %i@." l c;
    for i = 0 to l do
      for j = 0 to c do
        let d = diff m i j in
        match d with
        | None ->
            Format.fprintf ppf "    "
        | Some diff ->
            let sdiff = match diff with
              | Insert _ -> "\u{2190}"
              | Delete _ -> "\u{2191}"
              | Keep _ -> "\u{2196}"
              | Change _ -> "\u{21F1}"
            in
            let w = weight m i j in
            Format.fprintf ppf "%s%i " sdiff w
      done;
      Format.pp_print_newline ppf ()
    done

end


(* Building the patch.

   We first select the best final cell. A potential final cell
   is a cell where the local shape (i.e., the size of the strings) correspond
   to its position in the matrix. In other words: it's at the end of both its
   strings. We select the final cell with the smallest weight.

   We then build the patch by walking backward from the final cell to the
   origin.
*)

let select_final_state m0 =
  let maybe_final i j =
    match Matrix.shape_at m0 i j with
    | Some shape_here -> shape_here.l = i && shape_here.c = j
    | None -> false
  in
  let best_state (i0,j0,weigth0) (i,j) =
    let weight = Matrix.weight m0 i j in
    if weight < weigth0 then (i,j,weight) else (i0,j0,weigth0)
  in
  let res = ref (0,0,max_int) in
  let shape = Matrix.shape m0 in
  for i = 0 to shape.l do
    for j = 0 to shape.c do
      if maybe_final i j then
        res := best_state !res (i,j)
    done
  done;
  let i_final, j_final, _ = !res in
  assert (i_final <> 0 || j_final <> 0);
  (i_final, j_final)

let construct_patch m0 =
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

(* Computation of new cells *)

let select_best_proposition l =
  let compare_proposition curr prop =
    match curr, prop with
    | None, o | o, None -> o
    | Some (curr_m, curr_res), Some (m, res) ->
        Some (if curr_m <= m then curr_m, curr_res else m,res)
  in
  List.fold_left compare_proposition None l

  module type Full_core = sig
    type update_result
    type update_state
    val weight: change -> int
    val test: state -> left -> right -> (eq, diff) result
    val update: change -> update_state -> update_result
  end

module Generic
    (X: Full_core
     with type update_result := full_state
      and type update_state := full_state) = struct
  open X

  (* Boundary cell update *)
  let compute_column0  tbl i =
    let*! st = Matrix.state tbl (i-1) 0 in
    let*! line = Matrix.line tbl (i-1) 0 in
    let diff = Delete line in
    Matrix.set tbl i 0
      ~weight:(weight diff + Matrix.weight tbl (i-1) 0)
      ~state:(update diff st)
      ~diff:(Some diff)

  let compute_line0 tbl j =
    let*! st = Matrix.state tbl 0 (j-1) in
    let*! column = Matrix.column tbl 0 (j-1) in
    let diff = Insert column in
    Matrix.set tbl 0 j
      ~weight:(weight diff + Matrix.weight tbl 0 (j-1))
      ~state:(update diff st)
      ~diff:(Some diff)

let compute_inner_cell tbl i j =
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
    select_best_proposition [diag;del;insert]
  in
  let state = update diff localstate in
  Matrix.set tbl i j ~weight:newweight ~state ~diff:(Some diff)

let compute_cell  m i j =
  match i, j with
  | _ when Matrix.diff m i j <> None -> ()
  | 0,0 -> ()
  | 0,j -> compute_line0 m j
  | i,0 -> compute_column0  m i;
  | _ -> compute_inner_cell m i j

(* Filling the matrix

   We fill the whole matrix, as in vanilla Wagner-Fischer.
   At this point, the lists in some states might have been extended.
   If any list have been extended, we need to reshape the matrix
   and repeat the process
*)
let compute_matrix state0 =
  let m0 = Matrix.make { l = 0 ; c = 0 } in
  Matrix.set m0 0 0 ~weight:0 ~state:state0 ~diff:None;
  let rec loop m =
    let shape = Matrix.shape m in
    let new_shape = Matrix.real_shape m in
    if new_shape.l > shape.l || new_shape.c > shape.c then
      let m = Matrix.reshape new_shape m in
      for i = 0 to new_shape.l do
        for j = 0 to new_shape.c do
          compute_cell m i j
        done
      done;
      loop m
    else
      m
  in
  loop m0
 end


  module type Parameters = Full_core with type update_state := state

  module Simple(X:Parameters with type update_result := state) = struct
    module Internal = Generic(struct
        let test = X.test
        let weight = X.weight
        let update d fs = { fs with state = X.update d fs.state }
      end)

    let diff state line column =
      let fullstate = { line; column; state } in
      Internal.compute_matrix fullstate
      |> construct_patch
  end


  let may_append x = function
    | [||] -> x
    | y -> Array.append x y


  module Left_variadic
      (X:Parameters with type update_result := state * left array) = struct
    open X

    module Internal = Generic(struct
        let test = X.test
        let weight = X.weight
        let update d fs =
          let state, a = update d fs.state in
          { fs with state ; line = may_append fs.line a }
      end)

    let diff state line column =
      let fullstate = { line; column; state } in
      Internal.compute_matrix fullstate
      |> construct_patch
  end

  module Right_variadic
      (X:Parameters with type update_result := state * right array) = struct
    open X

    module Internal = Generic(struct
        let test = X.test
        let weight = X.weight
        let update d fs =
          let state, a = update d fs.state in
          { fs with state ; column = may_append fs.column a }
      end)

    let diff state line column =
      let fullstate = { line; column; state } in
      Internal.compute_matrix fullstate
      |> construct_patch
  end

end
