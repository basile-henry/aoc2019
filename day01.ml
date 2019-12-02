let simple_fuel mass = Int.div mass 3 - 2
let sumWith f data =
    List.fold_left
      (fun acc x -> acc + f x)
      0
      (List.filter_map int_of_string_opt (String.split_on_char '\n' data))

let problem1 =
    sumWith simple_fuel

let fuel mass = max 0 (simple_fuel mass)
let rec fix_fuel x =
    if x == 0 then 0 else x + fix_fuel (fuel x)

let problem2 =
    sumWith (fun mass -> fix_fuel (fuel mass))
