type ram = int array ref
type program =
  { ram : ram
  ; relative_base : int ref
  }
type input = int list
type output =
  { out_values : int list
  ; program_counter : int
  }

let resize (ram : ram) (size : int) : unit =
    let len = Array.length !ram in
    if size >= len then
      let delta = 1 + size - len in
      ram := Array.append !ram (Array.make delta 0)
    else ()

(* Index and possibly resize RAM *)
let read (ram : ram) (ix : int) : int =
    resize ram ix;
    !ram.(ix)

let write (ram : ram) (ix : int) (x: int) : unit =
    resize ram ix;
    !ram.(ix) <- x

(* Step through the program as the computer *)
let eval' (program : program) (initial_pc : int) : input -> output =
    let program_counter = ref initial_pc in
    let rec go (input : int list) =
        let next () =
            let out = read program.ram (!program_counter) in
            incr program_counter;
            out in

        let instr = next () in
        let opcode = instr mod 100 in
        let mode offset = (Int.div instr (offset * 100)) mod 10 in

        let param mode_offset =
            let arg = next () in
            match mode mode_offset with
              | 0 -> read program.ram arg (* position mode *)
              | 1 -> arg (* immediate mode *)
              | 2 -> read program.ram (arg + !(program.relative_base)) (* relative mode *)
              | _ -> failwith "Unexpected param mode" in
        let output mode_offset =
            let arg = next () in
            match mode mode_offset with
              | 0 -> arg (* position mode *)
              | 2 -> (arg + !(program.relative_base)) (* relative mode *)
              | _ -> failwith "Unexpected output mode" in

        let apply f =
            let a = param 1 in
            let b = param 10 in
            let result = f a b in
            write program.ram (output 100) result;
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
                write program.ram (output 1) x;
                go xs in
          out
        | 4 -> (* output *)
          let out_value = param 1 in
          let out = go input in
          { out with out_values = out_value :: out.out_values }
        | 5 -> jump (fun x -> x <> 0)
        | 6 -> jump (fun x -> x = 0)
        | 7 -> apply (fun a b -> if a < b then 1 else 0)
        | 8 -> apply (fun a b -> if a = b then 1 else 0)
        | 9 -> (* update relative base *)
            let arg = param 1 in
            program.relative_base := !(program.relative_base) + arg;
            go input
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

let simple_program (ram : int array) : program =
    { ram = ref ram; relative_base = ref 0 }

let copy_program (program : program) : program =
  { ram = ref (Array.copy !(program.ram))
  ; relative_base = ref !(program.relative_base)
  }

let parse_program (data : string) : program =
    data
    |> String.trim
    |> String.split_on_char ','
    |> List.filter_map int_of_string_opt
    |> Array.of_list
    |> simple_program

let run_program (data : string) (noun : int) (verb : int) : int =
    let program = parse_program data in
    !(program.ram).(1) <- noun;
    !(program.ram).(2) <- verb;
    eval program;
    !(program.ram).(0)

(* Helper to print a program *)
let print_program (program : program) =
    print_string "[ ";
    Array.iter
      (fun x ->
        print_int x;
        print_string "; ")
      !(program.ram);
    print_string "] ";
    print_int !(program.relative_base);
    print_newline ()

(* Helper to print an example *)
let print_example (index : int) (program : program) (should_be : program) =
    Printf.printf "Example %d:\n" index;
    print_string "program: ";
    print_program program;
    print_string "should be: ";
    print_program should_be;
    Printf.printf "same: %b\n\n" (program = should_be)

let example1 () =
    let program = simple_program [| 1; 0; 0; 0; 99; |] in
    let should_be = simple_program [| 2; 0; 0; 0; 99; |] in
    eval program;
    print_example 1 program should_be

let example2 () =
    let program = simple_program [| 2; 3; 0; 3; 99; |] in
    let should_be = simple_program [| 2; 3; 0; 6; 99; |] in
    eval program;
    print_example 2 program should_be

let example3 () =
    let program = simple_program [| 2; 4; 4; 5; 99; 0 |] in
    let should_be = simple_program [| 2; 4; 4; 5; 99; 9801; |] in
    eval program;
    print_example 3 program should_be

let example4 () =
    let program = simple_program [| 1; 1; 1; 4; 99; 5; 6; 0; 99; |] in
    let should_be = simple_program [| 30; 1; 1; 4; 2; 5; 6; 0; 99; |] in
    eval program;
    print_example 4 program should_be

let example5 () =
    let program = simple_program [| 3; 0; 4; 0; 99; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example6 () =
    let program = simple_program [| 3; 9; 8; 9; 10; 9; 4; 9; 99; -1; 8|] in
    Printf.printf "%b\n" ((eval' program 0 [4]).out_values = [0])

let example7 () =
    let program = simple_program
      [| 3; 12; 6; 12; 15; 1; 13; 14; 13; 4; 13; 99; -1; 0; 1; 9; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example8 () =
    let program = simple_program
      [| 3; 3; 1105; -1; 9; 1101; 0; 0; 12; 4; 12; 99; 1; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [1])

let example9 () =
    let program = simple_program
        [| 3; 21; 1008; 21; 8; 20; 1005; 20; 22; 107; 8; 21; 20; 1006; 20; 31;
        1106; 0; 36; 98; 0; 0; 1002; 21; 125; 20; 4; 20; 1105; 1; 46; 104;
        999; 1105; 1; 46; 1101; 1000; 1; 20; 4; 20; 1105; 1; 46; 98; 99; |] in
    Printf.printf "%b\n" ((eval' program 0 [1]).out_values = [999])

let example10 () =
  let output =
      [ 109; 1; 204; -1; 1001; 100; 1; 100; 1008; 100; 16; 101; 1006; 101; 0; 99; ] in
  let program = simple_program (Array.of_list output) in
  Printf.printf "%b\n" ((eval' program 0 []).out_values = output)

let example11 () =
  let program = simple_program
      [| 1102; 34915192; 34915192; 7; 4; 7; 99; 0; |] in
  Printf.printf "%b\n" ((eval' program 0 []).out_values = [1219070632396864])

let example12 () =
  let program = simple_program
      [| 104; 1125899906842624; 99; |] in
  Printf.printf "%b\n" ((eval' program 0 []).out_values = [1125899906842624])

let example13 () =
  let program = simple_program
      [| 109; 5; 203; 5; 4; 10; 99; 0; |] in
  Printf.printf "%b\n" ((eval' program 0 [473]).out_values = [473]);
