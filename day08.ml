let chunk (size : int) (xs : 'a array) : 'a array list =
    let xs_len = Array.length xs in
    let rec go offset =
        if offset >= Array.length xs
        then []
        else
          let len = min (xs_len - offset) size in
          Array.sub xs offset len :: go (offset + len) in
    go 0

let parse (w : int) (h : int) (data : string) : int array list =
    data
    |> String.trim
    |> String.to_seq
    |> Seq.map (fun c -> Char.code c - Char.code '0')
    |> Array.of_seq
    |> chunk (w * h)

let count (x : int) : int array -> int =
    Array.fold_left (fun c y -> if x = y then c + 1 else c) 0

let minimumBy (f : 'a -> int) (xs : 'a list) : 'a option =
    List.fold_left
      (fun myo x ->
        match myo with
        | (_, None) -> (f x, Some x)
        | (m, Some _) ->
          if f x < m
          then (f x, Some x)
          else myo)
      (0, None) xs
      |> snd

let print_layer (w : int) =
    Array.iteri
      (fun i x ->
        print_int x;
        if (i + 1) mod w = 0
        then print_newline ()
        else ())

let problem1 (data : string) =
  let min_zeros =
      data
      |> parse 25 6
      |> minimumBy (count 0) in
  match min_zeros with
  | None -> failwith "Unexpected"
  | Some layer ->
    count 1 layer * count 2 layer

let merge_pixel (a : int) (b : int) =
    if a = 2
    then b
    else a

let merge : int array -> int array -> int array =
    Array.map2 merge_pixel

let print_image (w : int) =
    Array.iteri
      (fun i x ->
        if x = 1
        then print_char '#'
        else print_char ' ';

        if (i + 1) mod w = 0
        then print_newline ()
        else ())

let problem2 (data : string) =
    let image =
        data
        |> parse 25 6
        |> List.fold_left merge (Array.make (25 * 6) 2) in
    print_image 25 image;
    0 (* dummy output *)
