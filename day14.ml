module StrMap = Map.Make(
  struct
    let compare = compare;
    type t = string
  end)

type input = (string * int)
type reactions = (int * input list) StrMap.t

let parse_input (str : string) : input =
    match String.split_on_char ' ' (String.trim str) with
    | [n; c] -> (c, int_of_string n)
    | _ -> failwith "Unexpected input"

let parse_reaction (line : string) : (string * (int * input list)) =
    match String.split_on_char '=' line with
    | [input; output] ->
        let (c, n) = parse_input (String.sub output 2 (String.length output - 2)) in
        let inputs =
            input
            |> String.split_on_char ','
            |> List.map parse_input in
        (c, (n, inputs))
    | _ -> failwith "Unexpected line"

let parse (data : string) : reactions =
    data
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map parse_reaction
    |> List.to_seq
    |> StrMap.of_seq

let insert k x m =
    StrMap.update k
      (function
      | None -> Some x
      | Some y -> Some (x + y)) m

let react reactions goal amount supply need =
    if amount > 0 then
      let (n, ingredients) = StrMap.find goal reactions in
      let mult = 1 + ((amount - 1) / n) in
      let extra_amount = mult * n - amount in
      if extra_amount > 0 then
        supply := insert goal extra_amount !supply;
      List.iter
        (fun (c, n) ->
          need := insert c (n * mult) !need)
        ingredients

let use_supply goal amount supply =
    match StrMap.find_opt goal !supply with
    | None -> amount
    | Some n ->
      if n >= amount then
        let () = supply := StrMap.add goal (n - amount) !supply in
        0
      else
        let () = supply := StrMap.remove goal !supply in
        amount - n

let num_ore reactions num_fuel =
    let need = ref (StrMap.singleton "FUEL" num_fuel) in
    let supply = ref StrMap.empty in
    let ore_count = ref 0 in

    while not (StrMap.is_empty !need) do
      let current_needs = StrMap.bindings !need in
      need := StrMap.empty;
      List.iter
        (function
          | ("ORE", amount) ->
            ore_count := !ore_count + amount
          | (x, amount) ->
            let amount' = use_supply x amount supply in
            react reactions x amount' supply need)
        current_needs;
    done;

    !ore_count

let problem1 (data : string) : int =
    let reactions = parse data in
    num_ore reactions 1

let rec search_fuel_amount reactions target_ore lower_bound upper_bound =
    if lower_bound = upper_bound - 1 then
      lower_bound
    else
      let test = (lower_bound + upper_bound) / 2 in
      let ore = num_ore reactions test in
      if ore > target_ore then
        search_fuel_amount reactions target_ore lower_bound test
      else if ore < target_ore then
        search_fuel_amount reactions target_ore test upper_bound
      else
        test

let problem2 (data : string) : int =
    let reactions = parse data in
    let target_ore = 1_000_000_000_000 in
    let lower_bound = target_ore / problem1 data in
    let upper_bound = target_ore in
    search_fuel_amount reactions target_ore lower_bound upper_bound
