let () =
    let day = int_of_string Sys.argv.(1) in
    let part = int_of_string Sys.argv.(2) in
    let data () =
        Std.input_all (open_in (Printf.sprintf "data/day%02d.txt" day)) in
    let print_problem x =
        print_int x;
        print_newline () in
    match (day, part) with
    | (1, 1) -> print_problem (Day01.problem1 (data ()))
    | (1, 2) -> print_problem (Day01.problem2 (data ()))
    | (2, 1) -> print_problem (Day02.problem1 (data ()))
    | (2, 2) -> print_problem (Day02.problem2 (data ()))
    | (3, 1) -> print_problem (Day03.problem1 (data ()))
    | (3, 2) -> print_problem (Day03.problem2 (data ()))
    | (4, 1) -> print_problem (Day04.problem1 ())
    | (4, 2) -> print_problem (Day04.problem2 ())
    | (5, 1) -> Day05.problem1 (data ())
    | (5, 2) -> Day05.problem2 (data ())
    | (6, 1) -> print_problem (Day06.problem1 (data ()))
    | (6, 2) -> print_problem (Day06.problem2 (data ()))
    | (7, 1) -> print_problem (Day07.problem1 (data ()))
    | (7, 2) -> print_problem (Day07.problem2 (data ()))
    | (8, 1) -> print_problem (Day08.problem1 (data ()))
    | (8, 2) -> Day08.problem2 (data ())
    | (9, 1) -> print_problem (Day09.problem1 (data ()))
    | (9, 2) -> print_problem (Day09.problem2 (data ()))
    | (10, 1) -> print_problem (Day10.problem1 (data ()))
    | (10, 2) -> print_problem (Day10.problem2 (data ()))
    | (11, 1) -> print_problem (Day11.problem1 (data ()))
    | (11, 2) -> Day11.problem2 (data ())
    | (12, 1) -> print_problem (Day12.problem1 (data ()))
    | (12, 2) -> print_problem (Day12.problem2 (data ()))
    | _ -> failwith "Not implemented"
