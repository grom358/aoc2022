open Core

type tile = Open | Wall | None

let tile_equal a b =
  match (a, b) with
  | Open, Open -> true
  | Wall, Wall -> true
  | None, None -> true
  | _ -> false

type direction = TurnLeft | TurnRight
type instruction = Steps of int | Turn of direction
type face = Up | Right | Down | Left

let offset face =
  match face with
  | Up -> (0, -1)
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)

let turn face direction =
  match (face, direction) with
  | Up, TurnLeft -> Left
  | Right, TurnLeft -> Up
  | Down, TurnLeft -> Right
  | Left, TurnLeft -> Down
  | Up, TurnRight -> Right
  | Right, TurnRight -> Down
  | Down, TurnRight -> Left
  | Left, TurnRight -> Up

let parse_map list =
  let parse_line line =
    String.to_list line
    |> List.map ~f:(fun c ->
           match c with
           | ' ' -> None
           | '.' -> Open
           | '#' -> Wall
           | _ -> failwith "invalid map character")
  in
  let max_cols =
    List.fold list ~init:0 ~f:(fun acc line -> max acc (String.length line))
  in
  List.map list ~f:(fun line ->
      let row = parse_line line in
      let padding = List.init (max_cols - List.length row) ~f:(fun _ -> None) in
      row @ padding |> List.to_array)
  |> List.to_array

let parse_instructions line =
  let pattern = "\\([0-9]+\\|[RL]\\)" in
  let regex = Str.regexp pattern in
  let parts =
    Str.full_split regex line
    |> List.map ~f:(function
         | Str.Text text -> text
         | Str.Delim delimiter -> delimiter)
  in
  let rec parse_parts parts acc =
    match parts with
    | [] -> failwith "invalid instructions"
    | num :: [] -> List.rev (Steps (Int.of_string num) :: acc)
    | num :: letter :: rest ->
        let direction =
          match letter with
          | "L" -> TurnLeft
          | "R" -> TurnRight
          | _ -> failwith "invalid direction"
        in
        parse_parts rest (Turn direction :: Steps (Int.of_string num) :: acc)
  in
  parse_parts parts []

let parse_file file_name =
  let input =
    In_channel.read_all file_name
    |> Str.split (Str.regexp "\n\n")
    |> List.map ~f:String.split_lines
  in
  match input with
  | [ map; [ instructions ] ] -> (parse_map map, parse_instructions instructions)
  | _ -> failwith "invalid input"

let in_bounds map x y =
  let rows = Array.length map and cols = Array.length map.(0) in
  x >= 0 && x <= cols - 1 && y >= 0 && y <= rows - 1

let wrap map (x, y) direction =
  let rec wrap' map x y dx dy =
    let new_x = x - dx and new_y = y - dy in
    if not (in_bounds map new_x new_y) then (x, y, direction)
    else
      match map.(new_y).(new_x) with
      | None -> (x, y, direction)
      | _ -> wrap' map new_x new_y dx dy
  in
  let x_offset, y_offset = offset direction in
  wrap' map x y x_offset y_offset

let follow_map map instructions wrap =
  let x = ref 0 and y = ref 0 and face = ref Right in
  let start_row = map.(0) in
  let idx, _ = Array.findi_exn start_row ~f:(fun _ t -> tile_equal t Open) in
  x := idx;
  let move steps =
    let rec move' steps =
      if steps > 0 then
        let dx, dy = offset !face in
        let new_x = !x + dx and new_y = !y + dy in
        let new_tile =
          if in_bounds map new_x new_y then map.(new_y).(new_x) else None
        in
        match new_tile with
        | Open ->
            x := new_x;
            y := new_y;
            move' (steps - 1)
        | Wall -> ()
        | None -> (
            let new_x, new_y, new_face = wrap map (!x, !y) !face in
            match map.(new_y).(new_x) with
            | Open ->
                x := new_x;
                y := new_y;
                face := new_face;
                move' (steps - 1)
            | Wall -> ()
            | None -> failwith "invalid logic")
    in
    move' steps
  in
  let rec follow = function
    | [] -> ()
    | instruction :: rest ->
        (match instruction with
        | Steps steps -> move steps
        | Turn direction -> face := turn !face direction);
        follow rest
  in
  follow instructions;
  let face_value =
    match !face with Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3
  in
  (1000 * (!y + 1)) + (4 * (!x + 1)) + face_value

let part1 map instructions = follow_map map instructions wrap

let wrap_cube _map (x, y) direction =
  (* This only works on cube given by input.text which has layout:
   *   1 2
   *   3
   * 4 5
   * 6
   *
   * Where 1 is top of cube,
   *
   * Front view:  Back view:
   *     1            1
   *  4  3  2      4  6  2
   *     5            5 
   *
   * I used a paper cutout of the layout and folded the paper to calculate
   * the mapping table.
   *)
  let cube_row, cube_col, new_direction =
    match (y / 50, x / 50, direction) with
    | 0, 1, Up -> (3, 0, Right)
    | 0, 1, Left -> (2, 0, Right)
    | 0, 2, Up -> (3, 0, Up)
    | 0, 2, Right -> (2, 1, Left)
    | 0, 2, Down -> (1, 1, Left)
    | 1, 1, Right -> (0, 2, Up)
    | 1, 1, Left -> (2, 0, Down)
    | 2, 0, Up -> (1, 1, Right)
    | 2, 0, Left -> (0, 1, Right)
    | 2, 1, Right -> (0, 2, Left)
    | 2, 1, Down -> (3, 0, Left)
    | 3, 0, Right -> (2, 1, Up)
    | 3, 0, Down -> (0, 2, Down)
    | 3, 0, Left -> (0, 1, Down)
    | _ -> failwith "unreachable"
  in
  let row_idx, col_idx = (y % 50, x % 50) in
  let i =
    match direction with
    | Left -> 49 - row_idx
    | Right -> row_idx
    | Up -> col_idx
    | Down -> 49 - col_idx
  in
  let new_row =
    match new_direction with
    | Left -> 49 - i
    | Right -> i
    | Up -> 49
    | Down -> 0
  in
  let new_col =
    match new_direction with
    | Left -> 49
    | Right -> 0
    | Up -> i
    | Down -> 49 - i
  in
  let new_x = (cube_col * 50) + new_col and new_y = (cube_row * 50) + new_row in
  (new_x, new_y, new_direction)

let part2 map instructions = follow_map map instructions wrap_cube

let () =
  let map, instructions = parse_file "input.txt" in
  let part1_answer = part1 map instructions in
  let part2_answer = part2 map instructions in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
