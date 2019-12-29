type position =
  { x : int
  ; y : int
  }

module PosMap = Map.Make(
  struct
    let compare a b = 2 * (compare a.x b.x) + compare a.y b.y
    type t = position
  end)

module PosSet = Set.Make(
  struct
    let compare a b = 2 * (compare a.x b.x) + compare a.y b.y
    type t = position
  end)

type direction = L | U | D | R

let range (range_start : int) (range_end : int) : int list =
    if range_end > range_start
    then List.init (range_end - range_start) (fun i -> range_start + i)
    else List.init (range_start - range_end) (fun i -> range_start - i)


let gcd (x : int) (y : int) : int =
    let rec go (x : int) (y : int) : int =
        if y = 0 then x else go y (x mod y) in
    go (abs x) (abs y)

let lcm (x : int) (y : int) : int =
    match (x, y) with
    | (_, 0) -> 0
    | (0, _) -> 0
    | _      -> abs ((Int.div x (gcd x y)) * y)

let sum =
    List.fold_left (fun x acc -> x + acc) 0
