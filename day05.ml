let solve (input : int) (data : string) =
    let program = Intcode.parse_program data in
    List.iter
      (fun x ->
        print_int x;
        print_newline ())
      (Intcode.eval' program [input])

let problem1 = solve 1

let problem2 = solve 5
