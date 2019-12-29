open Util

type game =
    { score : int
    ; tiles : int PosMap.t
    }

let empty_game =
    { score = 0
    ; tiles = PosMap.empty
    }

let rec parse (game : game) : int list -> game = function
    | [] -> game
    | -1::0::score::rest ->
      parse
        { game with score }
        rest
    | x::y::t::rest ->
      parse
        { game with tiles = PosMap.add {x; y} t game.tiles }
        rest
    | _ -> failwith "Unexpected number of items to parse"

let num_blocks (game : game) : int =
    PosMap.fold
      (fun _key a count ->
        match a with
        | 2 -> count + 1
        | _ -> count)
      game.tiles
      0

let problem1 (data : string) : int =
    let program = Intcode.parse_program data in
    let out = Intcode.eval' program [] in
    num_blocks (parse empty_game out)

let max_x m = PosMap.fold (fun key _ acc -> max key.x acc) m 0

let max_y m = PosMap.fold (fun key _ acc -> max key.y acc) m 0

let print_game (game : game) : unit =
    (range 0 (max_y game.tiles + 1))
    |> List.iter
        (fun y ->
          (range 0 (max_x game.tiles + 1))
          |> List.iter
              (fun x ->
                match PosMap.find_opt {x; y} game.tiles with
                | Some 0 -> print_char ' '
                | Some 1 -> print_char '+'
                | Some 2 -> print_char '#'
                | Some 3 -> print_char '='
                | Some 4 -> print_char 'o'
                | _ -> print_char ' ');
          print_newline ());
    Printf.printf "%d\n\n" game.score

let pos (game : game) obj : position =
    game.tiles
    |> PosMap.bindings
    |> List.find (fun (_, o) -> o = obj)
    |> fst

let rec play (program : Intcode.program) (game : game) move : int =
    let out = Intcode.eval' program [move] in
    let game = parse game out in
    print_game game;

    if num_blocks game = 0 then
      game.score
    else
      let next_move = compare (pos game 4) (pos game 3) in
      play program game next_move

let problem2 (data : string) : int =
    let program = Intcode.parse_program data in
    Intcode.write program.ram 0 2;
    play program empty_game 0
