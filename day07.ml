let rec amplifiers program setting input : int =
    match setting with
    | [] -> failwith "Expected non empty setting"
    | [x] -> List.hd (Intcode.eval' (Array.copy program) [x; input;])
    | x::xs ->
      let y = List.hd (Intcode.eval' (Array.copy program) [x; input;]) in
      amplifiers program xs y

let settings () : int list list =
    let rec go (pred : int list) : int list list =
        if List.length pred = 5
        then [pred]
        else
          Util.range 0 5
          |> List.filter (fun i -> not (List.mem i pred))
          |> List.map (fun i -> go (List.cons i pred))
          |> List.concat in
    go []

let problem1 (data : string) =
    let program = Intcode.parse_program data in
    List.fold_left
      (fun acc setting -> max acc (amplifiers program setting 0))
      0
      (settings ())

let problem2 (_data : string) = 42
