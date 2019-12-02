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
let example (index : int) (program : int array) (should_be : int array) =
    Printf.printf "Example %d:\n" index;
    print_string "program: ";
    print_program program;
    print_string "should be: ";
    print_program should_be;
    Printf.printf "same: %b\n\n" (program = should_be)

(* Step through the program as the computer *)
let rec step (program : int array ref) (pointer : int) =
    let get off = !program.(pointer + off) in
    let paddingTo x = Array.make (max 0 (1 + x - Array.length !program)) 0 in
    let apply f =
        let output = get 3 in
        let result = f !program.(get 1) !program.(get 2) in
        program := Array.append !program (paddingTo output);
        !program.(output) <- result;
        step program (pointer + 4) in
    match get 0 with
    | 1 -> apply Int.add
    | 2 -> apply Int.mul
    | 99 -> ()
    | x -> failwith (Printf.sprintf "Invalid opcode %d at pointer %d" x pointer)

let example1 () =
    let program = ref [| 1; 0; 0; 0; 99; |] in
    let should_be = [| 2; 0; 0; 0; 99; |] in
    step program 0;
    example 1 !program should_be

let example2 () =
    let program = ref [| 2; 3; 0; 3; 99; |] in
    let should_be = [| 2; 3; 0; 6; 99; |] in
    step program 0;
    example 2 !program should_be

let example3 () =
    let program = ref [| 2; 4; 4; 5; 99; |] in
    let should_be = [| 2; 4; 4; 5; 99; 9801; |] in
    step program 0;
    example 3 !program should_be

let example4 () =
    let program = ref [| 1; 1; 1; 4; 99; 5; 6; 0; 99; |] in
    let should_be = [| 30; 1; 1; 4; 2; 5; 6; 0; 99; |] in
    step program 0;
    example 4 !program should_be

let problem1 (data : string) =
    let split_data = String.split_on_char ',' (String.trim data) in
    let ops = List.filter_map int_of_string_opt split_data in
    let program = ref (Array.of_list ops) in
    !program.(1) <- 12;
    !program.(2) <- 2;
    step program 0;
    !program.(0)
