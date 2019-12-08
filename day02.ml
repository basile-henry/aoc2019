let problem1 (data : string) =
    Intcode.run_program data 12 2

let problem2 (data : string) =
    let goal = 19690720 in
    let combinations =
        List.concat (List.init 99
            (fun noun -> List.init 99
                (fun verb -> (noun, verb)))) in
    let (noun, verb) =
        List.find
          (fun (noun, verb) -> goal = Intcode.run_program data noun verb)
          combinations in
    noun * 100 + verb
