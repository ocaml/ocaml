(* Insert load/stores for pseudoregs that got assigned to stack locations.
   Insert moves to comply with calling conventions, etc. *)

open Misc
open Reg
open Mach

let redo_regalloc = ref false

let access_stack r =
  try
    for i = 0 to Array.length r - 1 do
      match r.(i).loc with Stack _ -> raise Exit | _ -> ()
    done;
    false
  with Exit ->
    true

let makereg r =
  match r.loc with
    Unknown -> fatal_error "Reload.makereg"
  | Reg _ -> r
  | Stack _ -> redo_regalloc := true; Reg.clone r

let makeregs rv =
  let n = Array.length rv in
  let newv = Array.new n Reg.dummy in
  for i = 0 to n-1 do newv.(i) <- makereg rv.(i) done;
  newv

let insert_move src dst next =
  if src.loc = dst.loc
  then next
  else instr_cons (Iop Imove) [|src|] [|dst|] next

let insert_moves src dst next =
  let rec insmoves i =
    if i >= Array.length src
    then next
    else insert_move src.(i) dst.(i) (insmoves (i+1))
  in insmoves 0

let rec reload i =
  match i.desc with
    Iend | Ireturn | Iop Itailcall_ind | Iop(Itailcall_imm _) | Iraise -> i
  | Iop(Imove | Ireload | Ispill) ->
      (* Do something if this is a stack-to-stack move *)
      begin match i.arg.(0), i.res.(0) with
        {loc = Stack s1}, {loc = Stack s2} when s1 <> s2 ->
          let r = makereg i.arg.(0) in
          insert_move i.arg.(0) r (insert_move r i.res.(0) (reload i.next))
      | _ ->
          instr_cons i.desc i.arg i.res (reload i.next)
      end
  | Iop op ->
      (* Let the machine description tell us whether some arguments / results
         can reside on the stack *)
      let (newarg, newres) =
        try
          Proc.reload_operation makereg op i.arg i.res
        with Proc.Use_default ->
          (* By default, assume that arguments and results must reside
             in hardware registers *)
          (makeregs i.arg, makeregs i.res) in
      insert_moves i.arg newarg
        (instr_cons_live i.desc newarg newres i.live
          (insert_moves newres i.res
            (reload i.next)))
  | Iifthenelse(tst, ifso, ifnot) ->
      (* Let the machine description tell us whether some arguments / results
         can reside on the stack *)
      let newarg =
        try
          Proc.reload_test makereg tst i.arg
        with Proc.Use_default ->
          makeregs i.arg in
      insert_moves i.arg newarg      
        (instr_cons (Iifthenelse(tst, reload ifso, reload ifnot)) newarg [||]
          (reload i.next))
  | Iswitch(index, cases) ->
      let newarg = makeregs i.arg in
      insert_moves i.arg newarg      
        (instr_cons (Iswitch(index, Array.map reload cases)) newarg [||]
          (reload i.next))
  | Iloop body ->
      instr_cons (Iloop(reload body)) [||] [||] (reload i.next)
  | Icatch(body, handler) ->
      instr_cons (Icatch(reload body, reload handler)) [||] [||]
        (reload i.next)
  | Iexit ->
      instr_cons Iexit [||] [||] dummy_instr
  | Itrywith(body, handler) ->
      instr_cons (Itrywith(reload body, reload handler)) [||] [||]
        (reload i.next)

let fundecl f =
  redo_regalloc := false;
  let new_body = reload f.fun_body in
  ({fun_name = f.fun_name; fun_args = f.fun_args;
    fun_body = new_body; fun_fast = f.fun_fast},
   !redo_regalloc)

