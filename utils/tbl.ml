type ('a, 'b) t =
    Empty
  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

let empty = Empty

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let new l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = height l and hr = height r in
  if hl > hr + 1 then
    let (Node(ll, lv, ld, lr, _)) = l in
    if height ll >= height lr then
      new ll lv ld (new lr x d r)
    else
      let (Node(lrl, lrv, lrd, lrr, _)) = lr in
      new (new ll lv ld lrl) lrv lrd (new lrr x d r)
  else if hr > hl + 1 then
    let (Node(rl, rv, rd, rr, _)) = r in
    if height rr >= height rl then
      new (new l x d rl) rv rd rr
    else
      let (Node(rll, rlv, rld, rlr, _)) = rl in
      new (new l x d rll) rlv rld (new rlr rv rd rr)
  else
    new l x d r

let rec add x data = function
    Empty ->
      Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) as t ->
      let c = compare x v in
      if c = 0 then
        Node(l, x, data, r, h)
      else if c < 0 then
        bal (add x data l) v d r
      else
        bal l v d (add x data r)

let rec find x = function
    Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

open Format

let print print_key print_data tbl =
  open_hovbox 2;
  print_string "[[";
  iter (fun k d ->
          open_hovbox 2;
          print_key k; print_string " ->"; print_space();
          print_data d; print_string ";";
          close_box())
        tbl;
  print_string "]]";
  close_box()
