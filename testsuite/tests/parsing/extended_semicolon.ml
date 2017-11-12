(* State: A virtual bot on an integer grid with an ASCII pen. *)

type world = {
  dim: int * int;
  trace: bytes;
  pos: int * int;
  vel: int * int;
  pen: char;
}

let init (w, h) =
  {dim = (w, h); trace = Bytes.make (w * h) ' '; pos = (0, 0); vel = (0, 0); pen = '-'}

let dump {dim = (w, h); trace; _} =
  for y = 0 to h - 1; do
    print_endline (Bytes.sub_string trace (y * w) w)
  done

(* State Monad: Control and kinetics. *)

type _ reg =
  | Pos : (int * int) reg
  | Vel : (int * int) reg
  | Pen : char reg

type _ action =
  | Hold : (unit -> 'a) -> 'a action
  | Bind : 'a action * ('a -> 'b action) -> 'b action
  | Put : 'a reg * 'a -> unit action
  | Get : 'a reg -> 'a action
  | Step : unit action

(* Monadic return and state handling. *)
let return x = Hold (fun () -> x)
let (:=) r x = Put (r, x)
let get r = Get r

(* Monadic bind and map. *)
let (>>=) m mf = Bind (m, mf)
let (>|=) m f = Bind (m, (fun x -> Hold (fun () -> f x)))

(* Statement variants of bind and map. This is an approximation, would normally
 * be implemented in PPX. *)
let (>>;) m m' = Bind (m, (fun () -> m'))
let (>|;) m s = m

(* An alias for ";" which mixes better with the above. Without support from a
 * PPX the evaluation is closer to plain wrong than an approximation. *)
let (-;) s v = v

let rec run : type a. a action -> world -> a * world = function
 | Hold f -> fun world -> (f (), world)
 | Bind (m, f) -> fun world -> let x, world' = run m world in run (f x) world'
 | Get Pos -> fun world -> (world.pos, world)
 | Get Vel -> fun world -> (world.vel, world)
 | Get Pen -> fun world -> (world.pen, world)
 | Put (Pos, pos) -> fun world -> ((), {world with pos})
 | Put (Vel, vel) -> fun world -> ((), {world with vel})
 | Put (Pen, pen) -> fun world -> ((), {world with pen})
 | Step -> fun world ->
    let (w, h), (x, y), (vx, vy) = world.dim, world.pos, world.vel in
    Bytes.set world.trace (x + w * y) world.pen;
    let (x', y') = ((x + vx + w) mod w, (y + vy + h) mod h) in
    ((), {world with pos = (x', y')})

(* Main: Drawing Triangles *)

let rec repeat n m =
  (* The semicolon operators have higher precedence than conditions, so this
   * works as intended: *)
  if n > 0 then
    m >>;
    repeat (n - 1) m
  else
    assert (n = 0) -;
    return ()

let draw_triangle n =
  get Pos >>= fun (xI, yI) ->

  (* The (>>;) operator has lower precedence than (:=). *)
  Vel := (1, 1) >>;
  repeat n Step >>;
  Vel := (1, -1) >>;
  repeat n Step >>;
  Vel := (-1, 0) >>;
  repeat (2 * n) Step >>;

  (* The (>>=) operator has significantly higher precedence than (>>;), but the
   * following still groups as expected, since (>>;) associates to the right,
   * and the right hand side of (>>=) will be delimited by the lambda. *)
  get Pos >>= fun (xF, yF) ->
  Vel := (0, 0) >>;
  assert (xF = xI && yF = yI) -; (* Voilà, no parentheses! *)
  Pos := (0, 0)

let draw_text w s =
  get Pos >>= fun (x0, _) ->
  Vel := (1, 0) >>;
  let rec aux i c =
    if i = String.length s then
      return ()
    else if c > w && s.[i] = ' ' then
      get Pos >>= fun (_, y) ->
      Pos := (x0, y + 1) >>;
      aux (i + 1) 0
    else
      Pen := s.[i] >>;
      Step >>;
      aux (i + 1) (c + 1) in
  aux 0 0

let test =
  Pos := (2, 9) >>;
  Pen := '-' >>;
  draw_triangle 8 >>;

  Pos := (4, 6) >>;
  Pen := '+' >>;
  draw_triangle 10 >>;

  Pen := '#' >>;
  Pos := (6, 1) >>;
  draw_triangle 14 >>;

  Pos := (29, 11) >>;
  draw_text 10 "These are not three triangles along a third dimension." >|;

  (* If we had a proper rewriter for (>|;) this statement would have been
   * sequenced into the monad. *)
  assert true

let () = (72, 19) |> init |> run test |> snd |> dump
