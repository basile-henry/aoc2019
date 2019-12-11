open Util

let rec amplifiers program setting input : int =
    match setting with
    | [] -> failwith "Expected non empty setting"
    | [x] -> List.hd (Intcode.eval' (Intcode.copy_program program) [x; input;])
    | x::xs ->
      let y = List.hd (Intcode.eval' (Intcode.copy_program program) [x; input;]) in
      amplifiers program xs y

let feedback_amplifiers program setting : int =
    let (amps : Intcode.program list) =
        List.map
          (fun s ->
            let p = Intcode.copy_program program in
            let _ = Intcode.eval' p [s] in
            p)
          setting in
    let rec row prev_output = function
        | [] -> Some prev_output
        | p::ps ->
          let out = Intcode.eval' p [prev_output] in
          match out with
          | [] -> None
          | [x] -> row x ps
          | _ -> failwith "Expected one output only" in
    let rec go last_amp_output =
        match row last_amp_output amps with
        | None -> last_amp_output
        | Some x -> go x in
    go 0

let settings (start : int) : int list list =
    let rec go (pred : int list) : int list list =
        if List.length pred = 5
        then [pred]
        else
          range start (start + 5)
          |> List.filter (fun i -> not (List.mem i pred))
          |> List.map (fun i -> go (List.cons i pred))
          |> List.concat in
    go []

let problem1 (data : string) =
    let program = Intcode.parse_program data in
    List.fold_left
      (fun acc setting -> max acc (amplifiers program setting 0))
      0
      (settings 0)

let problem2 (data : string) =
    let program = Intcode.parse_program data in
    List.fold_left
      (fun acc setting -> max acc (feedback_amplifiers program setting))
      0
      (settings 5)
