open Util

type tile = Wall | Empty | Oxygen
type state =
    { shortest_moves : int option
    ; map : tile PosMap.t
    ; pos : position
    ; dist_from_origin : int
    }

let init_state =
    { shortest_moves = None
    ; map = PosMap.empty
    ; pos = { x = 0; y = 0 }
    ; dist_from_origin = 0
    }

let move pos dir =
    match dir with
    | 1 -> { pos with y = pos.y - 1 }
    | 2 -> { pos with y = pos.y + 1 }
    | 3 -> { pos with x = pos.x - 1 }
    | 4 -> { pos with x = pos.x + 1 }
    | _ -> failwith "Unexpected dir"

let reverse_dir = function
    | 1 -> 2
    | 2 -> 1
    | 3 -> 4
    | 4 -> 3
    | _ -> failwith "Unexpected dir"

let all_dirs = [1;2;3;4]

let print_state state =
    let (from_x, to_x, from_y, to_y) =
        PosMap.fold
          (fun key _ (f_x, t_x, f_y, t_y) ->
            ( min key.x f_x
            , max key.x t_x
            , min key.y f_y
            , max key.y t_y
            ))
        state.map
        (0, 0, 0, 0) in

    Printf.printf "Shortest move: %d\n" (Option.default (-1) state.shortest_moves);
    Printf.printf "pos: (%d, %d)\n" state.pos.x state.pos.y;
    Printf.printf "dist_from_origin: %d\n" state.dist_from_origin;
    Printf.printf "map:\n";

    (range from_y (to_y + 1))
    |> List.iter
        (fun y ->
          (range from_x (to_x + 1))
          |> List.iter
              (fun x ->
                if {x; y} = state.pos then
                  print_char 'D'
                else
                  match PosMap.find_opt {x; y} state.map with
                  | Some Wall -> print_char '#'
                  | Some Empty -> print_char '.'
                  | Some Oxygen -> print_char 'o'
                  | None -> print_char '-');
          print_newline ());

    Printf.printf "\n"

let rec search (program : Intcode.program) state = function
    | [] -> state
    | dir::dirs ->
        let status = Intcode.eval' program [dir] in

        let step state tile =
            let cur_pos = state.pos in
            let cur_dist = state.dist_from_origin in
            let new_pos = move state.pos dir in
            let state =
                { state with
                  pos = new_pos
                ; map = PosMap.add new_pos tile state.map
                ; dist_from_origin = state.dist_from_origin + 1
                } in
            let next_dirs =
                List.filter (fun x -> x != reverse_dir dir) all_dirs in
            let state = search program state next_dirs in
            let _ = Intcode.eval' program [reverse_dir dir] in
            let state =
                { state with
                  pos = cur_pos
                ; dist_from_origin = cur_dist
                } in
            state in

        let state =
            match status with
            | [0] ->
              { state with map = PosMap.add (move state.pos dir) Wall state.map }
            | [1] -> step state Empty
            | [2] ->
              let dist = state.dist_from_origin + 1 in
              let state = step state Oxygen in
              { state with
                shortest_moves =
                match state.shortest_moves with
                | None -> Some dist
                | Some s -> Some (min s dist)
              }
              (* print_state state; *)
            | _ -> failwith "Unexpected status" in

        search program state dirs

let problem1 (data : string) : int =
    let program = Intcode.parse_program data in
    let state = search program init_state all_dirs in
    Option.get state.shortest_moves

let problem2 (_data : string) : int =
    failwith "todo"
