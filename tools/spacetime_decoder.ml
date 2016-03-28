let () =
  if Array.length Sys.argv <> 3 then begin
    failwith "Syntax: spacetime_decoder <executable> <pathname of profile>"
  end;
  let module S = Spacetime_lib in
  let series = S.Series.create ~executable:Sys.argv.(1) Sys.argv.(2) in
  List.iteri (fun index snapshot ->
      Printf.printf "Snapshot %d:\n" index;
      let entries =
        S.Snapshot.entries_sorted_by_words_highest_first snapshot
      in
      let module E = S.Entries_sorted_by_words_highest_first in
      E.iter (fun entry ->
        let blocks = S.Entry.blocks entry in
        let words = S.Entry.words entry in
        let backtrace = S.Entry.backtrace entry in
        Printf.printf "%d words in %d blocks allocated in " words blocks;
        let first = ref true in
        List.iter (fun location ->
            let module L = S.Location in
            let address = L.address location in
            let symbol = L.symbol location in
            let position = L.position location in
            let foreign = L.foreign location in
            if !first then begin
              first := false
            end else begin
              Printf.printf "  called from "
            end;
            let symbol_str =
              match symbol with
              | None -> ""
              | Some symbol -> Printf.sprintf " (symbol %s)" symbol
            in
            if foreign then begin
              Printf.printf "non-OCaml code at 0x%Lx%s\n" address symbol_str
            end else begin
              match position with
              | None ->
                Printf.printf "OCaml code at 0x%Lx%s\n" address symbol_str
              | Some position ->
                let module P = S.Position in
                let filename = P.filename position in
                let line = P.line_number position in
                let start_char = P.start_char position in
                let end_char = P.end_char position in
                Printf.printf "%s" filename;
                if not (line = 0 && start_char = 0 && end_char = 0) then begin
                  Printf.printf ":%d characters %d--%d\n" line
                    start_char end_char
                end else begin
                  Printf.printf " (exact location unknown)\n"
                end
            end)
          backtrace)
        entries)
    series
