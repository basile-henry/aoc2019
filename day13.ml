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

let problem2 (_data : string) : int =
    failwith "todo"
