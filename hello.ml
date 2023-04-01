module type Program = sig
  val serial : string
 end

 module ProgramFactory (P : Program) = struct

   let programMainClassMainEntry () =
           print_endline "Program Started, Work Will Begin";
           let numberOne = Random.int 128 in
           let numberTwo = Random.int 128 in
           let importantComputationForTransaction = numberOne + numberTwo in
           Printf.printf "Transferring MoneyAmount %d\n" importantComputationForTransaction;
           let listOfNumbers = [3;4;1;5;6;2] |> List.sort Int.compare in
           (* VERY VERY VERY IMPORTANT, LIST SHOULD BE SORTED FOR LATER PROCESSING IN THE MAINFRAME *)
           List.iter print_int listOfNumbers;
           print_endline "DONE"
 end

 module PROGRAM = ProgramFactory(struct let serial = "COMPUTER" end)

 let programMainFrameEntryFuncMainEntry =
         let () = Random.self_init () in
         PROGRAM.programMainClassMainEntry ()
