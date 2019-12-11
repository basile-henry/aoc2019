open Util

let gcd (a : int) (b : int) : int =
    let rec go (a : int) (b : int) : int =
        if b = 0 then a else go b (a mod b) in
    go (abs a) (abs b)

type ratio = (int * int)

let angle ((a, b) : ratio) : float =
    let o = atan2 (float_of_int a) (float_of_int (~- b)) in
    if o < 0. then
      o +. 2. *. Float.pi
    else
      o

module RatioMap = Map.Make(
  struct
    let compare a b =
          if a = b then
            0
          else
            compare (angle a) (angle b)
    type t = ratio
  end)


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

let maximumBy (f : position -> int) (set : PosSet.t) : (position option * int) =
    PosSet.fold
      (fun a (o, m) ->
        let b = f a in
        if b > m then (Some(a), b) else (o, m))
      set
      (None, 0)

let problem1 (data : string) =
    let asteroids = parse data in
    snd (maximumBy (num_visible asteroids) asteroids)

let manhattan_distance a b =
    abs (a.x - b.x) + abs (a.y - b.y)

let by_ratios (pos : position) (asteroids : PosSet.t) : position list RatioMap.t =
    let rec insert_manhattan x = function
        | [] -> [x]
        | y::ys ->
          if manhattan_distance x pos > manhattan_distance y pos then
            y :: insert_manhattan x ys
          else
            x :: y :: ys in

    let insert asteroid =
        RatioMap.update (direction pos asteroid)
          (function
            | None -> Some([asteroid])
            | Some(xs) -> Some(insert_manhattan asteroid xs)) in

    let others = PosSet.remove pos asteroids in

    PosSet.fold insert others RatioMap.empty

let update_all (f : 'a option -> 'a option) (map : 'a RatioMap.t) : 'a RatioMap.t =
    let keys = List.map fst (RatioMap.bindings map) in
    List.fold_right
      (fun key -> RatioMap.update key f)
      keys
      map

let rec ratio_index (ix : int) (ratios : position list RatioMap.t) : position =
    let size = RatioMap.cardinal ratios in
    if ix >= size then
      ratio_index
        (ix - size)
        (update_all
          (function
          | Some(_::x::xs) -> Some(x::xs)
          | _ -> None)
          ratios)
    else
      List.nth (RatioMap.bindings ratios) ix
      |> snd
      |> List.hd

let problem2 (data : string) =
    let asteroids = parse data in
    let center =
        maximumBy (num_visible asteroids) asteroids
        |> fst
        |> Option.get in
    let ratios = by_ratios center asteroids in
    let pos = ratio_index 199 ratios in
    pos.x * 100 + pos.y
