let rec rev_digits x =
    if x = 0
    then []
    else
      x mod 10 :: rev_digits (Int.div x 10)

(* Check if a number has monotonically increasing digits *)
let monotone_increase x =
    (* checking in reverse *)
    let rec go m digits =
      match digits with
      | [] -> true
      | d::ds -> m >= d && go d ds in
    go 9 (rev_digits x)

let has_same_consecutive x =
    let rec go digits =
      match digits with
      | [] -> false
      | _::[] -> false
      | a::b::xs -> a = b || go (b::xs) in
    go (rev_digits x)

let has_two_consecutive x =
    let rec go digits =
      match digits with
      | a::b::c::d::xs ->
        (a <> b && b = c && c <> d) || go (b::c::d::xs)
      | a::b::c::[] -> 
        a <> b && b = c
      | _ -> false in
    let digits = rev_digits x in
    match digits with
    | a::b::c::_ -> (a = b && a <> c) || go digits
    | _ -> failwith "Fewer than 3 digits"

let solve filter =
    let range_start = 402328 in
    let range_end = 864247 in
    let range =
      List.init
        (range_end - range_start)
        (fun i -> range_start + i) in
    let passwords = List.filter filter range in
    List.length passwords

let problem1 () =
    solve (fun x -> monotone_increase x && has_same_consecutive x)

let problem2 () =
    solve (fun x -> monotone_increase x && has_two_consecutive x)
