(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


(** Weak array operations *)

type 'a t;;

external create: int -> 'a t = "caml_weak_create";;

let length x = Obj.size(Obj.repr x) - 1;;

external set : 'a t -> int -> 'a option -> unit = "caml_weak_set";;
external get: 'a t -> int -> 'a option = "caml_weak_get";;
external get_copy: 'a t -> int -> 'a option = "caml_weak_get_copy";;
external check: 'a t -> int -> bool = "caml_weak_check";;

let fill ar ofs len x =
  if ofs < 0 || len < 0 || ofs + len > length ar
  then raise (Invalid_argument "Weak.fill")
  else begin
    for i = ofs to (ofs + len - 1) do
      set ar i x
    done
  end
;;

let blit ar1 of1 ar2 of2 len =
  if of1 < 0 || of1 + len > length ar1 || of2 < 0 || of2 + len > length ar2
  then raise (Invalid_argument "Weak.blit")
  else begin
    if of2 > of1 then begin
      for i = 0 to len - 1 do
        set ar2 (of2 + i) (get ar1 (of1 + i))
      done
    end else begin
      for i = len - 1 downto 0 do
        set ar2 (of2 + i) (get ar1 (of1 + i))
      done
    end
  end
;;


(** Weak hash tables *)

module type S = sig
  type data
  type t
  val create : int -> t
  val clear : t -> unit
  val merge : t -> data -> data
  val add : t -> data -> unit
  val remove : t -> data -> unit
  val find : t -> data -> data
  val find_all : t -> data -> data list
  val mem : t -> data -> bool
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
  val count : t -> int
  val stats : t -> int * int * int * int * int * int
end;;

module Make (H : Hashtbl.HashedType) : (S with type data = H.t) = struct

  type 'a weak_t = 'a t;;
  let weak_create = create;;
  let emptybucket = weak_create 0;;

  type data = H.t;;

  type t = {
    mutable table : data weak_t array;
    mutable totsize : int;             (* sum of the bucket sizes *)
    mutable limit : int;               (* max ratio totsize/table length *)
  };;

  let get_index t d = (H.hash d land max_int) mod (Array.length t.table);;

  let create sz =
    let sz = if sz < 7 then 7 else sz in
    let sz = if sz > Sys.max_array_length then Sys.max_array_length else sz in
    {
      table = Array.create sz emptybucket;
      totsize = 0;
      limit = 3;
    };;

  let clear t =
    for i = 0 to Array.length t.table - 1 do
      t.table.(i) <- emptybucket;
    done;
    t.totsize <- 0;
    t.limit <- 3;
  ;;

  let fold f t init =
    let rec fold_bucket i b accu =
      if i >= length b then accu else
      match get b i with
      | Some v -> fold_bucket (i+1) b (f v accu)
      | None -> fold_bucket (i+1) b accu
    in
    Array.fold_right (fold_bucket 0) t.table init
  ;;

  let iter f t =
    let rec iter_bucket i b =
      if i >= length b then () else
      match get b i with
      | Some v -> f v; iter_bucket (i+1) b
      | None -> iter_bucket (i+1) b
    in
    Array.iter (iter_bucket 0) t.table
  ;;

  let count t =
    let rec count_bucket i b accu =
      if i >= length b then accu else
      count_bucket (i+1) b (accu + (if check b i then 1 else 0))
    in
    Array.fold_right (count_bucket 0) t.table 0
  ;;

  let next_sz n = min (3*n/2 + 3) (Sys.max_array_length - 1);;

  let rec resize t =
    let oldlen = Array.length t.table in
    let newlen = next_sz oldlen in
    if newlen > oldlen then begin
      let newt = create newlen in
      newt.limit <- t.limit + 100;          (* prevent resizing of newt *)
      fold (fun d () -> add newt d) t ();
   (* assert Array.length newt.table = newlen; *)
      t.table <- newt.table;
      t.limit <- t.limit + 2;
    end

  and add_aux t d index =
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then begin
        let newsz = min (sz + 3) (Sys.max_array_length - 1) in
        if newsz <= sz then failwith "Weak.Make : hash bucket cannot grow more";
        let newbucket = weak_create newsz in
        blit bucket 0 newbucket 0 sz;
        set newbucket i (Some d);
        t.table.(index) <- newbucket;
        t.totsize <- t.totsize + (newsz - sz);
        if t.totsize > t.limit * Array.length t.table then resize t;
      end else begin
        if check bucket i
        then loop (i+1)
        else set bucket i (Some d)
      end
    in
    loop 0;

  and add t d = add_aux t d (get_index t d)
  ;;

  let find_or t d ifnotfound =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then ifnotfound index
      else begin
        match get_copy bucket i with
        | Some v when H.equal v d
           -> begin match get bucket i with
              | Some v -> v
              | None -> loop (i+1)
              end
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let merge t d = find_or t d (fun index -> add_aux t d index; d);;

  let find t d = find_or t d (fun index -> raise Not_found);;

  let find_shadow t d iffound ifnotfound =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i =
      if i >= sz then ifnotfound else begin
        match get_copy bucket i with
        | Some v when H.equal v d -> iffound bucket i
        | _ -> loop (i+1)
      end
    in
    loop 0
  ;;

  let remove t d = find_shadow t d (fun w i -> set w i None) ();;

  let mem t d = find_shadow t d (fun w i -> true) false;;

  let find_all t d =
    let index = get_index t d in
    let bucket = t.table.(index) in
    let sz = length bucket in
    let rec loop i accu =
      if i >= sz then accu
      else begin
        match get_copy bucket i with
        | Some v when H.equal v d
           -> begin match get bucket i with
              | Some v -> loop (i+1) (v::accu)
              | None -> loop (i+1) accu
              end
        | _ -> loop (i+1) accu
      end
    in
    loop 0 []
  ;;

  let stats t =
    let len = Array.length t.table in
    let lens = Array.map length t.table in
    Array.sort compare lens;
    let totlen = Array.fold_left ( + ) 0 lens in
    (len, count t, totlen, lens.(0), lens.(len/2), lens.(len-1))
  ;;

end;;
