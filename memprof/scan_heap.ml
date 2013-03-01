open HPTypes
open HPGlobals

let rec iter h p n =
  let b = h.hp_blocks.(p) in
  if not ! (b.block_scanned) then begin
      if b.block_tag >= Obj.no_scan_tag then begin
          b.block_scanned := true;
          n := !n + b.block_size;
        end else begin
          
(*      Printf.printf "Scanning block %ld size %d\n" p b.block_size; *)
          b.block_scanned := true;
          n := !n + b.block_size;
          for i = 0 to Array.length b.block_content - 1 do
            iter h b.block_content.(i) n
          done
        end
    end
    
let scan h p =
  Array.iter (fun b ->
      if ! (b.block_scanned) then b.block_scanned := false
  ) h.hp_blocks;
  let n = ref 0 in
  iter h p n;
(*  Printf.printf "SCANNED: %d <-> WEIGHT: %d"
    !n h.hp_blocks.(p).block_weight; print_newline (); *)
  !n
