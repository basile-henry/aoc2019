let rec amplifiers program setting input : int =
    match setting with
    | [] -> failwith "Expected non empty setting"
    | [x] -> List.hd (Intcode.eval' (Array.copy program) 0 [x; input;]).out_values
    | x::xs ->
      let y = List.hd (Intcode.eval' (Array.copy program) 0 [x; input;]).out_values in
      amplifiers program xs y

type amplifier =
  (int * Intcode.program) ref

let feedback_amplifiers program setting : int =
    let (amps : amplifier list) =
        List.map
          (fun s ->
            let p = Array.copy program in
            let out = Intcode.eval' p 0 [s] in
            ref (out.program_counter, p))
          setting in
    let rec row prev_output = function
        | [] -> Some prev_output
        | amp::ps ->
          let (pc, p) = !amp in
          let out = Intcode.eval' p pc [prev_output] in
          amp := (out.program_counter, p);
          match out.out_values with
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
          Util.range start (start + 5)
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
