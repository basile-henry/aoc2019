type position =
  { x : int
  ; y : int
  }

module PosSet = Set.Make(
  struct
    let compare a b = 2 * (compare a.x b.x) + compare a.y b.y
    type t = position
  end)

let gcd (a : int) (b : int) : int =
    let rec go (a : int) (b : int) : int =
        if b = 0 then a else go b (a mod b) in
    go (abs a) (abs b)

type ratio = (int * int)

let direction (a : position) (b : position) : ratio =
    let dx = b.x - a.x in
    let dy = b.y - a.y in
    let g = gcd dx dy in
    (dx / g, dy / g)

let visible (pos : position) (asteroid : position) (seen : ratio list) : bool =
    pos <> asteroid &&
    let ratio = direction pos asteroid in
    seen
    |> List.exists (fun seen_ratio -> seen_ratio = ratio)
    |> not

let num_visible (asteroids : PosSet.t) (pos : position) : int =
    List.length
      (PosSet.fold
        (fun asteroid seen ->
          if visible pos asteroid seen then
            List.cons (direction pos asteroid) seen
          else
            seen)
        asteroids
        [])

let parse (data : string) : PosSet.t =
    data
    |> String.trim
    |> String.split_on_char '\n'
    |> List.mapi (fun y row ->
       row
       |> String.to_seq
       |> List.of_seq
       |> List.mapi (fun x c ->
         ( { x = x; y = y; }
         , c
         )))
    |> List.concat
    |> List.filter_map (fun (pos, c) ->
       match c with
       | '.' -> None
       | '#' -> Some(pos)
       | _ -> failwith "Unsupported char")
    |> List.to_seq
    |> PosSet.of_seq

let maximumBy (f : position -> int) (set : PosSet.t) : int =
    PosSet.fold (fun a -> max (f a)) set 0

let problem1 (data : string) =
    let asteroids = parse data in
    maximumBy (num_visible asteroids) asteroids

let problem2 (_data : string) = 42
