let problem1 (data : string) =
    let program = Intcode.parse_program data in
    Intcode.eval program

let problem2 = problem1
