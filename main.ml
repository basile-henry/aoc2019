let () =
    let day = int_of_string Sys.argv.(1) in
    let part = int_of_string Sys.argv.(2) in
    let data =
        Std.input_all (open_in (Printf.sprintf "data/day%02d/input" day)) in
    let answer =
        match (day, part) with
        | (1, 1) -> Day01.problem1 data
        | (1, 2) -> Day01.problem2 data
        | (2, 1) -> Day02.problem1 data
        | _ -> failwith "Not implemented" in
    Printf.printf "%d\n" answer
