(* Construction of the interference graph.
   Annotate pseudoregs with interference lists and preference lists. *)

open Reg
open Mach

let build_graph fundecl =

  (* The interference graph is represented in two ways:
     - by adjacency lists for each register
     - by a triangular bit matrix *)

  let num_regs = Reg.num_registers() in
  let mat =
    String.make (((num_regs * (num_regs + 1)) lsr 1 + 7) lsr 3) '\000' in

  (* Record an interference between two registers *)
  let add_interf ri rj =
    let i = ri.stamp and j = rj.stamp in
    if i = j then () else begin
      let n = if i < j then ((j * (j + 1)) lsr 1) + i
                       else ((i * (i + 1)) lsr 1) + j in
      let b = Char.code(mat.[n lsr 3]) in
      let msk = 1 lsl (n land 7) in
      if b land msk = 0 then begin
        mat.[n lsr 3] <- Char.unsafe_chr(b lor msk);
        begin match ri.loc with
          Unknown -> ri.interf <- rj :: ri.interf | _ -> ()
        end;
        begin match rj.loc with
          Unknown -> rj.interf <- ri :: rj.interf | _ -> ()
        end
      end
    end in

  (* Record interferences between a register array and a set of registers *)
  let add_interf_set v s =
    for i = 0 to Array.length v - 1 do
      let r1 = v.(i) in
      Reg.Set.iter (add_interf r1) s
    done in

  (* Record interferences between elements of an array *)
  let add_interf_self v =
    for i = 0 to Array.length v - 2 do
      let ri = v.(i) in
      for j = i+1 to Array.length v - 1 do
        add_interf ri v.(j)
      done
    done in

  (* Record interferences between the destination of a move and a set
     of live registers. Since the destination is equal to the source,
     do not add an interference between them if the source is still live
     afterwards. *)
  let add_interf_move src dst s =
    Reg.Set.iter (fun r -> if r.stamp <> src.stamp then add_interf dst r) s in

  (* Add a preference from one reg to another *)
  let add_pref weight r1 r2 =
    if r1.stamp = r2.stamp then () else begin
      match r1.loc with
          Unknown -> r1.prefer <- (r2, weight) :: r1.prefer
        | _ -> ()
    end in

  (* Add a mutual preference between two regs *)
  let add_mutual_pref weight r1 r2 =
    add_pref weight r1 r2; add_pref weight r2 r1 in

  (* Update the spill cost of the registers involved in an operation *)

  let add_spill_cost cost arg =
    for i = 0 to Array.length arg - 1 do
      let r = arg.(i) in r.spill_cost <- r.spill_cost + cost
    done in

  (* Compute interferences, preferences and spill costs *)

  let rec interf weight i =
    let destroyed = Proc.destroyed_at_oper i.desc in
    if Array.length destroyed > 0 then add_interf_set destroyed i.live;
    add_spill_cost weight i.arg;
    add_spill_cost weight i.res;
    match i.desc with
      Iend -> ()
    | Ireturn -> ()
    | Iop(Imove) ->
        add_interf_move i.arg.(0) i.res.(0) i.live;
        add_mutual_pref weight i.arg.(0) i.res.(0);
        interf weight i.next
    | Iop(Ispill) ->
        add_interf_move i.arg.(0) i.res.(0) i.live;
        add_pref (weight / 4) i.arg.(0) i.res.(0);
        interf weight i.next
    | Iop(Ireload) ->
        add_interf_move i.arg.(0) i.res.(0) i.live;
        add_pref (weight / 4) i.res.(0) i.arg.(0);
        interf weight i.next
    | Iop(Itailcall_ind) -> ()
    | Iop(Itailcall_imm lbl) -> ()
    | Iop op ->
        add_interf_set i.res i.live;
        add_interf_self i.res;
        interf weight i.next
    | Iifthenelse(tst, ifso, ifnot) ->
        interf weight ifso; interf weight ifnot; interf weight i.next
    | Iswitch(index, cases) ->
        for i = 0 to Array.length cases - 1 do
          interf weight cases.(i)
        done;
        interf weight i.next
    | Iloop body ->
        interf (8 * weight) body; interf weight i.next
    | Icatch(body, handler) ->
        interf weight body; interf weight handler; interf weight i.next
    | Iexit ->
        ()
    | Itrywith(body, handler) ->
        add_interf_set Proc.destroyed_at_raise handler.live;    
        interf weight body; interf weight handler; interf weight i.next
    | Iraise -> ()
  in
  interf 8 fundecl.fun_body

