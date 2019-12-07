module OrbitMap = Map.Make(
  struct
    let compare = compare
    type t = string
  end)

let build_orbit_map : string list list -> string OrbitMap.t =
    let step map x =
        match x with
        | [c;o] -> OrbitMap.add o c map
        | _ -> failwith "Unexpected" in
    List.fold_left step OrbitMap.empty

let num_orbits (map : string OrbitMap.t) : int OrbitMap.t =
    let rec get_num orbits x =
        match OrbitMap.find_opt x orbits with
        | Some x -> (x, orbits)
        | None ->
            let y = OrbitMap.find x map in
            let (yn, yo) = get_num orbits y in
            let n = 1 + yn in
            (n, OrbitMap.add x n yo) in
    map
      |> OrbitMap.bindings
      |> List.map fst
      |> List.fold_left (fun o x -> snd (get_num o x)) (OrbitMap.singleton "COM" 0)

let total_orbits (map : int OrbitMap.t) : int =
    OrbitMap.fold (fun _ a b -> a + b) map 0

let setup_orbit_map (data : string) =
    data
      |> String.trim
      |> String.split_on_char '\n'
      |> List.map (String.split_on_char ')')
      |> build_orbit_map

let problem1 (data : string) =
    data
      |> setup_orbit_map
      |> num_orbits
      |> total_orbits

let rec path_to_com (start : string) (map : string OrbitMap.t) : string list =
    match start with
    | "COM" -> ["COM"]
    | _ ->
      let next = OrbitMap.find start map in
      start :: path_to_com next map

let rec prune_common_start (a: 'a list) (b: 'a list) : ('a list * 'a list) =
    match (a, b) with
    | (x::xs, y::ys) ->
      if x = y
      then prune_common_start xs ys
      else (a, b)
    | _ -> (a, b)

let print_list =
    List.iter
      (fun a ->
        print_string a;
        print_newline ())

let problem2 (data : string) =
    let orbit_map = setup_orbit_map data in
    let (a, b) =
        prune_common_start
          (path_to_com "YOU" orbit_map |> List.rev)
          (path_to_com "SAN" orbit_map |> List.rev) in
    List.length a + List.length b - 2
