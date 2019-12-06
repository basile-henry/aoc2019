
let range (range_start : int) (range_end : int) : int list =
    if range_end > range_start
    then List.init (range_end - range_start) (fun i -> range_start + i)
    else List.init (range_start - range_end) (fun i -> range_start - i)
