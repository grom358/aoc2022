open Core

let parse_file file_name =
  let map =
    In_channel.read_lines file_name
    |> List.map ~f:String.to_array
    |> List.to_array
  in
  let start_pos = ref (-1, -1) in
  let end_pos = ref (-1, -1) in
  for y = 0 to Array.length map - 1 do
    let row = map.(y) in
    for x = 0 to Array.length row - 1 do
      match row.(x) with
      | 'S' ->
          start_pos := (x, y);
          row.(x) <- 'a'
      | 'E' ->
          end_pos := (x, y);
          row.(x) <- 'z'
      | _ -> ()
    done
  done;
  (map, !start_pos, !end_pos)

let step a b = Char.to_int b - Char.to_int a

let find_neighbors map (x, y) =
  let n = Array.length map in
  let m = Array.length map.(0) in
  let c = map.(y).(x) in
  let candidates = [ (x - 1, y); (x, y + 1); (x + 1, y); (x, y - 1) ] in
  List.filter candidates ~f:(fun (i, j) ->
      if i >= 0 && i < m && j >= 0 && j < n then
        let distance = step c map.(j).(i) in
        distance <= 1
      else false)

module IntPair = struct
  module T = struct
    type t = int * int
    [@@deriving compare, sexp_of]
  end
  include T
  include Comparator.Make(T)
end

let bfs map start_pos end_pos =
  let rec bfs' queue visited =
    match queue with
    | [] -> None
    | (pos, path) :: rest ->
        if IntPair.compare pos end_pos = 0 then Some (List.rev path)
        else if Set.mem visited pos then bfs' rest visited
        else
          let neighbors =
            find_neighbors map pos
            |> List.filter ~f:(fun p -> not (Set.mem visited p))
          in
          let new_paths = List.map ~f:(fun p -> (p, p :: path)) neighbors in
          let new_queue = rest @ new_paths in
          let new_visited = Set.add visited pos in
          bfs' new_queue new_visited
  in
  let visited = Set.empty (module IntPair) in
  bfs' [ (start_pos, []) ] visited

let path_steps map from_pos to_pos =
  match bfs map from_pos to_pos with
  | Some path -> List.length path
  | None -> Int.max_value

let find_start_positions map =
  let positions = ref [] in
  for row = 0 to Array.length map - 1 do
    for col = 0 to Array.length map.(row) - 1 do
      if Char.equal map.(row).(col) 'a' then
        positions := (col, row) :: !positions
    done
  done;
  !positions

let part2 map end_pos =
  let start_positions = find_start_positions map in
  let rec part2' positions min_steps =
    match positions with
    | [] -> min_steps
    | pos :: rest ->
        let steps = path_steps map pos end_pos in
        let min_steps = Int.min steps min_steps in
        part2' rest min_steps
  in
  part2' start_positions Int.max_value

let () =
  let map, start_pos, end_pos = parse_file "input.txt" in
  let part1_answer = path_steps map start_pos end_pos in
  let part2_answer = part2 map end_pos in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
