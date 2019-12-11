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
