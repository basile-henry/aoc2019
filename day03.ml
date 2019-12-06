type direction = L | U | D | R
type movement =
  { direction : direction
  ; distance : int
  }
type position =
  { x : int
  ; y : int
  }

module PosSet = Set.Make( 
  struct
    let compare a b = 2 * (compare a.x b.x) + compare a.y b.y
    type t = position
  end )

let parse_wire (word : string) : movement =
    let to_dist xs = int_of_string (String.of_seq (List.to_seq xs)) in
    match List.of_seq (String.to_seq word) with
    | 'L'::dist ->
        { direction = L
        ; distance = to_dist dist
        }
    | 'U'::dist ->
        { direction = U
        ; distance = to_dist dist
        }
    | 'D'::dist ->
        { direction = D
        ; distance = to_dist dist
        }
    | 'R'::dist ->
        { direction = R
        ; distance = to_dist dist
        }
    | _ -> failwith "parse_wire: Failed"

let parse_wire : string list -> movement list =
    List.map parse_wire

let parse (data : string) : movement list list=
    List.map
      (fun line -> parse_wire (String.split_on_char ',' line))
      (String.split_on_char '\n' (String.trim data))

let movements_to_set : movement list -> PosSet.t =
    let rec go current set xs =
      match xs with
      | [] -> set
      | m::ms -> 
        let (segment, next) =
          match m.direction with
          | L ->
              let up_to = current.x - m.distance in
              ( Util.range current.x up_to
                  |> List.map (fun x -> { current with x = x } )
              , { current with x = up_to }
              )
          | U ->
              let up_to = current.y + m.distance in
              ( Util.range current.y up_to
                  |> List.map (fun y -> { current with y = y } )
              , { current with y = up_to }
              )
          | D ->
              let up_to = current.y - m.distance in
              ( Util.range current.y up_to
                  |> List.map (fun y -> { current with y = y } )
              , { current with y = up_to }
              )
          | R ->
              let up_to = current.x + m.distance in
              ( Util.range current.x up_to
                  |> List.map (fun x -> { current with x = x } )
              , { current with x = up_to }
              ) in
        let new_set =
            segment
              |> PosSet.of_list
              |> PosSet.union set in
        go next new_set ms in
    go { x = 0 ; y = 0 } PosSet.empty

let wire_intersections wire_a wire_b =
    PosSet.inter wire_b wire_a
      |> PosSet.remove { x = 0; y = 0 }
      |> PosSet.to_seq
      |> List.of_seq

(* Manhattan distance from origin *)
let manhattan_distance pos =
    abs pos.x + abs pos.y

let minimum xs =
    match xs with
    | [] -> failwith "No minimum"
    | x::xs -> List.fold_left min x xs

let problem1 (data : string) =
    let wires =
        data
          |> parse
          |> List.map movements_to_set
          |> Array.of_list in
    wire_intersections wires.(0) wires.(1)
      |> List.map manhattan_distance
      |> minimum
