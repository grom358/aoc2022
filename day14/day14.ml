open Core

let parse_file file_name =
  let parse_position pos_str =
    match String.split ~on:',' pos_str with
    | [ x; y ] -> (Int.of_string x, Int.of_string y)
    | _ -> failwith "Invalid position"
  in
  let parse_line line =
    Str.split (Str.regexp " -> ") line |> List.map ~f:parse_position
  in
  In_channel.read_lines file_name |> List.map ~f:parse_line

type tile = Rock | Air | Sand

let tile_to_char = function Rock -> '#' | Air -> '.' | Sand -> 'o'

let draw_line grid (x1, y1) (x2, y2) =
  for y = min y1 y2 to max y1 y2 do
    for x = min x1 x2 to max x1 x2 do
      grid.(y).(x) <- Rock
    done
  done

let create_grid paths =
  let rec draw_path grid path =
    match path with
    | a :: b :: rest ->
        draw_line grid a b;
        draw_path grid (b :: rest)
    | _ -> ()
  in
  let grid = Array.make_matrix ~dimx:200 ~dimy:1000 Air in
  List.iter paths ~f:(fun path -> draw_path grid path);
  grid

let drop_sand grid =
  let bottom_y = Array.length grid - 1 in
  let rec drop_sand' x y =
    if y >= bottom_y then None
    else
      match grid.(y).(x) with
      | Rock | Sand -> None
      | Air -> (
          match grid.(y + 1).(x) with
          | Air -> drop_sand' x (y + 1)
          | Rock | Sand -> (
              match grid.(y + 1).(x - 1) with
              | Air -> drop_sand' (x - 1) (y + 1)
              | Rock | Sand -> (
                  match grid.(y + 1).(x + 1) with
                  | Air -> drop_sand' (x + 1) (y + 1)
                  | Rock | Sand -> Some (x, y))))
  in
  drop_sand' 500 0

let _print_grid grid (x1, y1) (x2, y2) =
  let rows = Array.filteri ~f:(fun y _ -> y >= y1 && y <= y2) grid in
  Array.iter rows ~f:(fun row ->
      let cols = Array.filteri ~f:(fun x _ -> x >= x1 && x <= x2) row in
      let line = Array.map ~f:tile_to_char cols |> String.of_array in
      printf "%s\n" line)

let simulate grid =
  let rec simulate' grid =
    match drop_sand grid with
    | None -> grid
    | Some (x, y) ->
        grid.(y).(x) <- Sand;
        simulate' grid
  in
  simulate' grid

let count_sand grid =
  Array.fold grid ~init:0 ~f:(fun acc row ->
      acc + Array.count row ~f:(function Sand -> true | _ -> false))

let part1 paths = create_grid paths |> simulate |> count_sand

let part2 paths =
  let grid = create_grid paths in
  let max_y =
    List.fold paths ~init:0 ~f:(fun acc path ->
        List.fold path ~init:acc ~f:(fun acc (_, y) -> max acc y))
  in
  draw_line grid (0, max_y + 2) (Array.length grid.(0) - 1, max_y + 2);
  let answer = simulate grid |> count_sand in
  answer

let () =
  let paths = parse_file "input.txt" in
  let part1_answer = part1 paths and part2_answer = part2 paths in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
