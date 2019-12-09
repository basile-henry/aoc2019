let solve (x : int) (data : string) : int =
    let program = Intcode.parse_program data in
    List.hd (Intcode.eval' program 0 [x]).out_values

let problem1 = solve 1

let problem2 = solve 2
