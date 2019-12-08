type program = int array
type input = int list
type output =
  { out_values : int list
  ; program_counter : int
  }

(* Step through the program as the computer *)
let eval' (program : program) (initial_pc : int) : input -> output =
    let program_counter = ref initial_pc in
    let rec go (input : int list) =
        let next () =
            let out = program.(!program_counter) in
            program_counter := !program_counter + 1;
            out in
        let instr = next () in
        let opcode = instr mod 100 in
        let mode offset = (Int.div instr (offset * 100)) mod 10 in
        let param mode_offset =
            let arg = next () in
            match mode mode_offset with
              | 0 -> program.(arg) (* position mode *)
              | 1 -> arg (* immediate mode *)
              | _ -> failwith "Unexpected mode" in
        let apply f =
            let a = param 1 in
            let b = param 10 in
            let result = f a b in
            let output = next () in
            program.(output) <- result;
            go input in
        let jump pred =
            let test = param 1 in
            let jump_to = param 10 in
            if pred test
            then program_counter := jump_to
            else ();
            go input in
        match opcode with
        | 1 -> apply Int.add
        | 2 -> apply Int.mul
        | 3 -> (* input *)
          let out =
              match input with
              | [] ->
                { out_values = []
                ; program_counter = !program_counter - 1
                }
              | x::xs ->
                program.(next ()) <- x;
                go xs in
          out
        | 4 -> (* output *)
          let out_value = param 1 in
          let output = go input in
          { output with out_values = out_value :: output.out_values }
        | 5 -> jump (fun x -> x <> 0)
        | 6 -> jump (fun x -> x = 0)
        | 7 -> apply (fun a b -> if a < b then 1 else 0)
        | 8 -> apply (fun a b -> if a = b then 1 else 0)
        | 99 ->
            { out_values = []
            ; program_counter = !program_counter - 1
            }
        | x ->
          failwith
            (Printf.sprintf "Invalid opcode %d, pc %d" x !program_counter) in
    go

let eval (program : program) =
    let _ = eval' program 0 [] in
    ()

let parse_program (data : string) : program =
    data
    |> String.trim
    |> String.split_on_char ','
    |> List.filter_map int_of_string_opt
    |> Array.of_list

let run_program (data : string) (noun : int) (verb : int) : int =
    let program = parse_program data in
    program.(1) <- noun;
    program.(2) <- verb;
    eval program;
    program.(0)

(* Helper to print a program *)
let print_program (program : program) =
    print_string "[ ";
    Array.iter
      (fun x ->
        print_int x;
        print_string "; ")
      program;
    print_string "]\n"

(* Helper to print an example *)
let print_example (index : int) (program : int array) (should_be : int array) =
    Printf.printf "Example %d:\n" index;
    print_string "program: ";
    print_program program;
    print_string "should be: ";
    print_program should_be;
    Printf.printf "same: %b\n\n" (program = should_be)

let example1 () =
    let program = [| 1; 0; 0; 0; 99; |] in
    let should_be = [| 2; 0; 0; 0; 99; |] in
    eval program;
    print_example 1 program should_be

let example2 () =
    let program = [| 2; 3; 0; 3; 99; |] in
    let should_be = [| 2; 3; 0; 6; 99; |] in
    eval program;
    print_example 2 program should_be

let example3 () =
    let program = [| 2; 4; 4; 5; 99; 0 |] in
    let should_be = [| 2; 4; 4; 5; 99; 9801; |] in
    eval program;
    print_example 3 program should_be

let example4 () =
    let program = [| 1; 1; 1; 4; 99; 5; 6; 0; 99; |] in
    let should_be = [| 30; 1; 1; 4; 2; 5; 6; 0; 99; |] in
    eval program;
    print_example 4 program should_be

let example5 () =
    let program = [| 3; 0; 4; 0; 99; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example6 () =
    let program = [| 3; 9; 8; 9; 10; 9; 4; 9; 99; -1; 8|] in
    Printf.printf "%b\n" ((eval' program 0 [4]).out_values = [0])

let example7 () =
    let program =
      [| 3; 12; 6; 12; 15; 1; 13; 14; 13; 4; 13; 99; -1; 0; 1; 9; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example8 () =
    let program =
      [| 3; 3; 1105; -1; 9; 1101; 0; 0; 12; 4; 12; 99; 1; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example9 () =
    let program =
        [| 3; 21; 1008; 21; 8; 20; 1005; 20; 22; 107; 8; 21; 20; 1006; 20; 31;
        1106; 0; 36; 98; 0; 0; 1002; 21; 125; 20; 4; 20; 1105; 1; 46; 104;
        999; 1105; 1; 46; 1101; 1000; 1; 20; 4; 20; 1105; 1; 46; 98; 99; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [999])
