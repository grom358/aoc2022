open Core

type position = { x : int; y : int }

let create_position () = { x = 0; y = 0 }

let compare_positions p1 p2 =
  let cmp_x = Int.compare p1.x p2.x in
  if cmp_x <> 0 then cmp_x else Int.compare p1.y p2.y

let create_rope length = List.init length ~f:(fun _ -> create_position ())

let move_rope rope head_dx head_dy =
  let rec move_rope_knot rope head_dx head_dy new_rope =
    match rope with
    | [] -> List.rev new_rope
    | head :: [] ->
        let new_head = { x = head.x + head_dx; y = head.y + head_dy } in
        List.rev (new_head :: new_rope)
    | head :: next :: rest ->
        let new_head = { x = head.x + head_dx; y = head.y + head_dy } in
        let dx = new_head.x - next.x in
        let dy = new_head.y - next.y in
        let next_dx, next_dy =
          match (dx, dy) with
          (* T.H -> TH *)
          | 2, 0 -> (1, 0)
          (* H.T -> HT *)
          | -2, 0 -> (-1, 0)
          (* ..H    .TH
           * T.. -> ...
           *)
          | 2, 1 -> (1, 1)
          (* H..    HT.
           * ..T -> ...
           *)
          | -2, 1 -> (-1, 1)
          (* ..T    ... 
           * H.. -> HT.
           *)
          | -2, -1 -> (-1, -1)
          (* T..    ...
           * ..H -> .TH
           *)
          | 2, -1 -> (1, -1)
          (* H    H
           * . -> T
           * T    .
           *)
          | 0, 2 -> (0, 1)
          (* T    .
           * . -> T
           * H    H
           *)
          | 0, -2 -> (0, -1)
          (* .H    .H
           * .. -> .T
           * T.    ..
           *)
          | 1, 2 -> (1, 1)
          (* H.    H.
           * .. -> T. 
           * .T    ..
           *)
          | -1, 2 -> (-1, 1)
          (* .T    ..
           * .. -> T.
           * H.    H.
           *)
          | -1, -2 -> (-1, -1)
          (* T.    ..
           * .. -> .T
           * .H    .H
           *)
          | 1, -2 -> (1, -1)
          (* ..H    ..H
           * ... -> .T.
           * T..    ...
           *)
          | 2, 2 -> (1, 1)
          (* H..    H..
           * ... -> .T.
           * ..T    ...
           *)
          | -2, 2 -> (-1, 1)
          (* ..T    ...
           * ... -> .T.
           * H..    H..
           *)
          | -2, -2 -> (-1, -1)
          (* T..    ...
           * ... -> .T.
           * ..H    ..H
           *)
          | 2, -2 -> (1, -1)
          | _ -> (0, 0)
        in
        move_rope_knot (next :: rest) next_dx next_dy (new_head :: new_rope)
  in
  move_rope_knot rope head_dx head_dy []

let move_right rope = move_rope rope 1 0
let move_up rope = move_rope rope 0 1
let move_left rope = move_rope rope (-1) 0
let move_down rope = move_rope rope 0 (-1)

type move = RIGHT | UP | LEFT | DOWN

let follow_rope move_list rope_length =
  let rope = create_rope rope_length in
  let rec track_rope rope move_list visited =
    match move_list with
    | [] -> visited
    | hd :: tl ->
        let new_rope =
          match hd with
          | UP -> move_up rope
          | RIGHT -> move_right rope
          | DOWN -> move_down rope
          | LEFT -> move_left rope
        in
        let tail_position = List.last_exn new_rope in
        track_rope new_rope tl (tail_position :: visited)
  in
  track_rope rope move_list []

let tail_visited moves rope_length =
  follow_rope moves rope_length
  |> List.dedup_and_sort ~compare:compare_positions
  |> List.length

let part1 moves = tail_visited moves 2
let part2 moves = tail_visited moves 10

let parse_line line =
  match String.split ~on:' ' line with
  | [ move_char; repeat_char ] ->
      let move =
        match move_char with
        | "U" -> UP
        | "R" -> RIGHT
        | "D" -> DOWN
        | "L" -> LEFT
        | _ -> failwith "Invalid move"
      in
      let repeat = Int.of_string repeat_char in
      (move, repeat)
  | _ -> failwith "Invalid line"

let convert_moves lst =
  List.concat_map lst ~f:(fun (m, count) -> List.init count ~f:(fun _ -> m))

let () =
  let moves =
    In_channel.read_lines "input.txt" |> List.map ~f:parse_line |> convert_moves
  in
  let part1_answer = part1 moves in
  let part2_answer = part2 moves in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
