let problem1 (data : string) =
    let program = Intcode.parse_program data in
    Intcode.eval program;
    0 (* dummy return value *)

let problem2 (_data : string) = 42
