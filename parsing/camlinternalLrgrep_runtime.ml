type lr1 = int
type clause = int
type register = int

(* Representation of automaton as sparse tables and bytecoded programs *)

let get_uint24_be str i =
  (String.get_uint16_be str i) lor (String.get_uint8 str (i + 2) lsl 16)

module Sparse_table = struct
  type row = int
  type col = int
  type value = int

  let get1 str i =
    if i < 0 || i + 1 > String.length str
    then min_int
    else String.get_uint8 str i

  let get2 str i =
    let i = i * 2 in
    if i < 0 || i + 2 > String.length str
    then min_int
    else String.get_uint16_be str i

  let get3 str i =
    let i = i * 3 in
    if i < 0 || i + 3 > String.length str
    then min_int
    else get_uint24_be str i

  let get4 str i =
    let i = i * 4 in
    if i < 0 || i + 4 > String.length str
    then min_int
    else Int32.to_int (String.get_int32_be str i)

  type t = {
    displacement: int -> int;
    offset: int;
    keys: int -> int;
    values: int -> int;
  }

  let lookup coded (row : row) (col : col) : value option =
    assert (row >= 0 && col >= 0);
    let displacement = coded.displacement col in
    if displacement = min_int then
      None
    else
      let offset = displacement - coded.offset + row in
      if coded.keys offset = row + 1 then
        Some (coded.values offset)
      else
        None
end

type program_code = string
type program_counter = int

type priority = int

type program_instruction =
  | Store of register
  | Move of register * register
  | Swap of register * register
  | Clear of register
  | Yield of program_counter
  | Accept of clause * priority * register option array
  | Match of Sparse_table.row
  | Priority of clause * priority * priority
  | Halt

type 'a register_value =
  | Empty
  | Location of Lexing.position * Lexing.position
  | Value of 'a

type 'a register_values = 'a register_value array

let get_int table ~offset = function
  | 1 -> String.get_uint8 table offset
  | 2 -> String.get_uint16_be table offset
  | 3 -> (String.get_uint16_be table offset) lor
         (String.get_uint8 table (offset + 2) lsl 16)
  | 4 -> Int32.to_int (String.get_int32_be table offset)
  | _ -> assert false

let program_step (t : program_code) (r : program_counter ref)
  : program_instruction =
  let pc = !r in
  match t.[pc] with
  | '\x01' ->
    r := !r + 2;
    Store (String.get_uint8 t (pc + 1))
  | '\x02' ->
    r := !r + 3;
    Move (String.get_uint8 t (pc + 1), String.get_uint8 t (pc + 2))
  | '\x09' ->
    r := !r + 3;
    Swap (String.get_uint8 t (pc + 1), String.get_uint8 t (pc + 2))
  | '\x03' ->
    r := !r + 2;
    Clear (String.get_uint8 t (pc + 1))
  | '\x04' ->
    r := !r + 4;
    Yield (get_int t ~offset:(pc + 1) 3)
  | '\x05' ->
    let clause = String.get_uint16_be t (pc + 1) in
    let priority = String.get_uint8 t (pc + 3) in
    let arity = String.get_uint8 t (pc + 4) in
    let registers = Array.init arity (fun i ->
        let x = String.get_uint8 t (pc + 5 + i) in
        if x = 255 then None else Some x
      ) in
    r := !r + 5 + arity;
    Accept (clause, priority, registers)
  | '\x06' ->
    r := !r + 4;
    Match (get_uint24_be t (pc + 1))
  | '\x07' ->
    r := !r + 1;
    Halt
  | '\x08' ->
    r := !r + 5;
    Priority (String.get_uint16_be t (pc + 1),
              String.get_uint8 t (pc + 3),
              String.get_uint8 t (pc + 4))
  | x ->
    Printf.ksprintf failwith "Invalid opcode: %02X at 0x%04X" (Char.code x) pc

type program = {
  registers : int;
  initial : program_counter;
  table : Sparse_table.t;
  code : program_code;
}

module type Parser = sig
  type 'a env
  type element
  val current_state_number : 'a env -> int
  val top : 'a env -> element option
  val pop : 'a env -> 'a env option
  val positions: 'a env -> Lexing.position * Lexing.position
end

let debug = false

let eprintf = Printf.eprintf

let print_regs bank regs =
  Printf.sprintf "[%s]"
    (String.concat ", "
      (List.map (function
        | None -> "None"
        | Some i -> "%" ^ string_of_int i ^ " = " ^ match bank.(i) with
          | Empty -> "Empty"
          | Location _ -> "Location _"
          | Value _ -> "Value _"
      ) (Array.to_list regs)))

let add_candidate candidates ~clause ~priority registers bank =
  let may_get = function
    | None -> Empty
    | Some i -> bank.(i)
  in
  let mk () =
    let arguments = Array.map may_get registers in
    (clause, priority, arguments)
  in
  let rec loop = function
    | [] -> [mk ()]
    | ((clause', priority', _) :: xs) as xxs when clause = clause' ->
      if priority <= priority'
      then mk () :: xs
      else xxs
    | ((clause', _, _) :: _) as xxs when clause' > clause ->
      mk () :: xxs
    | x :: xs ->
      x :: loop xs
  in
  candidates := loop !candidates

let remap_candidate candidates ~(clause : clause) p1 p2 =
  let rec loop = function
    | (clause', p1', args) :: rest
      when clause' = clause && p1 = p1' ->
      (clause', p2, args) :: rest
    | ((clause', _, _) as x) :: xs when clause' < clause ->
      x :: loop xs
    | _ -> raise Not_found
  in
  match loop !candidates with
  | exception Not_found ->
    if debug then eprintf "Remap skipped\n";
    ()
  | candidates' ->
    if debug then eprintf "Remap applied\n";
    candidates := candidates'

let rec interpret_last program bank candidates pc =
  match program_step program.code pc with
  | Accept (clause, priority, registers) ->
    if debug then eprintf "Accept (%d,%s) (bottom)\n" clause (print_regs bank registers);
    add_candidate candidates ~clause ~priority registers bank;
    interpret_last program bank candidates pc
  | _ -> ()

type 'a candidate = clause * 'a register_values

module Interpreter (P : Parser) =
struct
  let interpret program bank env candidates (pc : program_counter) =
    let pc = ref pc in
    let rec loop () =
      match program_step program.code pc with
      | Store reg ->
        if debug then eprintf "Store %d\n" reg;
        bank.(reg) <- (match P.top env with
            | Some x -> Value x
            | None -> let s, e = P.positions env in Location (s, e));
        loop ()
      | Move (r1, r2) ->
        if debug then eprintf "Move %d -> %d\n" r1 r2;
        bank.(r2) <- bank.(r1);
        loop ()
      | Swap (r1, r2) ->
        if debug then eprintf "Swap %d <-> %d\n" r1 r2;
        let v2 = bank.(r2) in
        bank.(r2) <- bank.(r1);
        bank.(r1) <- v2;
        loop ()
      | Clear r1 ->
        if debug then eprintf "Clear %d\n" r1;
        bank.(r1) <- Empty;
        loop ()
      | Yield pc' ->
        if debug then prerr_endline "Yield";
        Some pc'
      | Accept (clause, priority, registers) ->
        if debug then eprintf "Accept (%d,%d,%s)\n"
            clause priority (print_regs bank registers);
        add_candidate candidates ~clause ~priority registers bank;
        loop ()
      | Match index ->
        let state = P.current_state_number env in
        let () = match Sparse_table.lookup program.table index state with
          | Some pc' ->
            if debug then
              eprintf "Match %d %d: success, jump to 0x%04X\n" index state pc';
            pc := pc'
          | None ->
            if debug then
              eprintf "Match %d %d: failure\n" index state
        in
        loop ()
      | Halt ->
        if debug then prerr_endline "Halt";
        None
      | Priority (clause, p1, p2) ->
        if debug then eprintf
            "Priority: clause %d remapped %d -> %d\n" clause p1 p2;
        remap_candidate candidates ~clause p1 p2;
        loop ()
    in
    loop ()

  let lrgrep_run program env =
    let bank = Array.make program.registers Empty in
    let candidates = ref [] in
    let rec loop env pc =
      match interpret program bank env candidates pc with
      | None -> ()
      | Some pc' ->
        match P.pop env with
        | None -> interpret_last program bank candidates (ref pc')
        | Some env -> loop env pc'
    in
    begin
      try
        loop env program.initial;
      with exn ->
        Printf.eprintf "LRgrep internal error: %s\n" (Printexc.to_string exn)
    end;
    List.map (fun (k,_,v) -> (k, v)) !candidates
end
