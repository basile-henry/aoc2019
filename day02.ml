(* Helper to print a program *)
let print_program (program : int array) =
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
let rec eval (program : int array) (pointer : int) =
    let get off = program.(pointer + off) in
    let apply f =
        let output = get 3 in
        let result = f program.(get 1) program.(get 2) in
        program.(output) <- result;
        eval program (pointer + 4) in
    match get 0 with
    | 1 -> apply Int.add
    | 2 -> apply Int.mul
    | 99 -> ()
    | x -> failwith (Printf.sprintf "Invalid opcode %d at pointer %d" x pointer)

let example1 () =
    let program = [| 1; 0; 0; 0; 99; |] in
    let should_be = [| 2; 0; 0; 0; 99; |] in
    eval program 0;
    print_example 1 program should_be

let example2 () =
    let program = [| 2; 3; 0; 3; 99; |] in
    let should_be = [| 2; 3; 0; 6; 99; |] in
    eval program 0;
    print_example 2 program should_be

let example3 () =
    let program = [| 2; 4; 4; 5; 99; 0 |] in
    let should_be = [| 2; 4; 4; 5; 99; 9801; |] in
    eval program 0;
    print_example 3 program should_be

let example4 () =
    let program = [| 1; 1; 1; 4; 99; 5; 6; 0; 99; |] in
    let should_be = [| 30; 1; 1; 4; 2; 5; 6; 0; 99; |] in
    eval program 0;
    print_example 4 program should_be

let run_program (data : string) (noun : int) (verb : int) =
    let split_data = String.split_on_char ',' (String.trim data) in
    let ops = List.filter_map int_of_string_opt split_data in
    let program = Array.of_list ops in
    program.(1) <- noun;
    program.(2) <- verb;
    eval program 0;
    program.(0)

let problem1 (data : string) =
    run_program data 12 2

let problem2 (data : string) =
    let goal = 19690720 in
    let combinations =
        List.concat (List.init 99
            (fun noun -> List.init 99
                (fun verb -> (noun, verb)))) in
    let (noun, verb) =
        List.find
          (fun (noun, verb) -> goal = run_program data noun verb)
          combinations in
    noun * 100 + verb
