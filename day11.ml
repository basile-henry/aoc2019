open Util

type colour = Black | White

type robot =
  { pos : position ref
  ; facing : direction ref
  ; painted : colour PosMap.t ref
  }

let initial_robot (start : colour) =
    let pos = ref { x = 0; y = 0 } in
    { pos
    ; facing = ref U
    ; painted = ref (PosMap.singleton !pos start)
    }

let panel_color (robot : robot) : colour =
    match PosMap.find_opt !(robot.pos) !(robot.painted) with
    | None -> Black
    | Some(c) -> c

let colour_encode : colour -> int = function
    | Black -> 0
    | White -> 1

let colour_decode : int -> colour = function
    | 0 -> Black
    | 1 -> White
    | _ -> failwith "Unexpected colour code"

let turn (turn_dir : int) (robot : robot) : unit =
    (match (turn_dir, !(robot.facing)) with
    | (0, R) | (1, L) -> robot.facing := U
    | (0, D) | (1, U) -> robot.facing := R
    | (0, L) | (1, R) -> robot.facing := D
    | (0, U) | (1, D) -> robot.facing := L
    | _ -> ());

    let {x; y} = !(robot.pos) in
    match !(robot.facing) with
    | U -> robot.pos := { x; y = y - 1 }
    | L -> robot.pos := { y; x = x - 1 }
    | D -> robot.pos := { x; y = y + 1 }
    | R -> robot.pos := { y; x = x + 1 }

let move_robot (start : colour) (program : Intcode.program) : robot =
    let robot = initial_robot start in

    let rec go () =

        let input = colour_encode (panel_color robot) in

        match Intcode.eval' program [input] with
        | [] -> ()
        | [paint; turn_dir] ->
          robot.painted := PosMap.add !(robot.pos) (colour_decode paint) !(robot.painted);
          turn turn_dir robot;
          go ()
        | _ -> failwith "Unexpected output"

        in
    go ();
    robot

let problem1 (data : string) : int =
    data
    |> Intcode.parse_program
    |> move_robot Black
    |> (fun robot -> !(robot.painted))
    |> PosMap.cardinal

let fold_paint (g : position -> int) (f : int -> int -> int) (paint : colour PosMap.t) : int =
    PosMap.fold
      (fun k _ -> function
        | None -> Some(g k)
        | Some(x) -> Some(f (g k) x))
      paint
      None
      |> Option.get

let print_paint (paint : colour PosMap.t) : unit =
    let start_y = fold_paint (fun pos -> pos.y) min paint in
    let end_y   = fold_paint (fun pos -> pos.y) max paint in
    let start_x = fold_paint (fun pos -> pos.x) min paint in
    let end_x   = fold_paint (fun pos -> pos.x) max paint in

    range start_y (end_y + 1)
    |> List.iter
        (fun y ->
          range start_x (end_x + 1)
          |> List.iter
              (fun x ->
                match PosMap.find_opt {x; y} paint with
                | Some(White) -> print_char '#'
                | _ -> print_char ' ');
          print_newline ())

let problem2 (data : string) : unit =
    let paint =
        data
        |> Intcode.parse_program
        |> move_robot White
        |> (fun robot -> !(robot.painted)) in
    print_paint paint
