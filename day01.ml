let simple_fuel mass = Int.div mass 3 - 2
let sumWith f = List.fold_left (fun acc x -> acc + f x) 0

let problem1 =
    sumWith (fun line -> simple_fuel (int_of_string line))

let fuel mass = max 0 (simple_fuel mass)
let rec fix_fuel x =
    if x == 0 then 0 else x + fix_fuel (fuel x)

let problem2 =
    sumWith (fun line -> fix_fuel (fuel (int_of_string line)))