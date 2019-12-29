open Util

type vec1 = int
type vec3 = int list
type planets1 = (vec1 list * vec1 list)
type planets3 = (vec3 list * vec3 list)

let parse (data : string) : planets3 =
    ( data
      |> String.trim
      |> String.split_on_char '\n'
      |> List.map (fun line ->
          match String.split_on_char '=' line with
          | [_;x;y;z] ->
              [ int_of_string (x |> String.split_on_char ',' |> List.hd)
              ; int_of_string (y |> String.split_on_char ',' |> List.hd)
              ; int_of_string (z |> String.split_on_char '>' |> List.hd)
              ]
          | _ -> failwith "Failed to parse line")
    , List.init 4 (fun _ -> List.init 3 (fun _ -> 0))
    )

let rec transpose : 'a list list -> 'a list list = function
    | [] -> []
    | [x] -> List.map (fun y -> [y]) x
    | x::xs -> List.map2 (fun y z -> y :: z) x (transpose xs)

let step_vel (pos : vec1 list) (p : vec1) (v : vec1) : vec1 =
    let plus = List.length (List.filter (fun x -> x > p) pos) in
    let minus = List.length (List.filter (fun x -> x < p) pos) in
    v + plus - minus

let step ((pos, vel) : planets1) : planets1 =
    let vel' = List.map2 (step_vel pos) pos vel in
    let pos' = List.map2 (fun p v -> p + v) vel' pos in
    (pos', vel')

let rec simulate1d (steps : int) (planets : planets1) : planets1 =
    match steps with
    | 0 -> planets
    | n -> simulate1d (n - 1) (step planets)

let simulate3d (steps : int) ((pos, vel) : planets3) : planets3 =
    let out =
        List.map2
          (fun p v -> simulate1d steps (p, v))
          (transpose pos)
          (transpose vel) in
    let p = transpose (List.map fst out) in
    let v = transpose (List.map snd out) in
    (p, v)

let total_energy ((pos, vel) : planets3) : int =
    let energy x = 
        x
        |> List.map (List.map abs)
        |> List.map sum in

    List.map2 Int.mul (energy pos) (energy vel)
    |> sum

let problem1 (data : string) : int =
    let (pos, vel) = data
      |> parse
      |> simulate3d 1000 in

    total_energy (pos, vel)

let period1 (planets : planets1) : int =
    let rec go n ps =
        if ps = planets
        then n
        else go (n + 1) (step ps) in
    go 1 (step planets)

let period3 ((pos, vel) : planets3) : int =
    let periods =
        List.map2
          (fun p v -> period1 (p, v))
          (transpose pos)
          (transpose vel) in
    List.fold_left lcm 1 periods

let problem2 (data : string) : int =
    data
    |> parse
    |> period3
