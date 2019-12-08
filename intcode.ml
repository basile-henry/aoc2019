type program = int array

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

(* Step through the program as the computer *)
let eval (program : program) : unit =
    let rec go (program_counter : int ref) =
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
            go program_counter in
        match opcode with
        | 1 -> apply Int.add
        | 2 -> apply Int.mul
        | 3 -> (* input *)
          program.(next ()) <- read_int ();
          go program_counter;
        | 4 -> (* output *)
          print_int (param 1);
          print_newline ();
          go program_counter;
        | 99 -> ()
        | x ->
          failwith
            (Printf.sprintf "Invalid opcode %d, pc %d" x !program_counter) in
    go (ref 0)

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
    eval program

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
