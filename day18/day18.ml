open Core

module Coord = struct
  module T = struct
    type t = int * int * int [@@deriving compare, sexp_of]

    let of_list = function
      | [ x; y; z ] -> (x, y, z)
      | _ -> failwith "Invalid coord"
  end

  include T
  include Comparator.Make (T)
end

let parse_file file_name =
  In_channel.read_lines file_name
  |> List.map ~f:(fun s -> String.split ~on:',' s |> List.map ~f:Int.of_string)
  |> List.map ~f:Coord.of_list
  |> Set.of_list (module Coord)

let neighbors (x, y, z) =
  [
    (x - 1, y, z);
    (x + 1, y, z);
    (x, y + 1, z);
    (x, y - 1, z);
    (x, y, z + 1);
    (x, y, z - 1);
  ]

let part1 coords =
  Set.fold coords ~init:0 ~f:(fun acc coord ->
      let connected =
        List.count (neighbors coord) ~f:(fun neighbor ->
            Set.mem coords neighbor)
      in
      acc + (6 - connected))

let bounds coords =
  Set.fold coords
    ~init:((Int.max_value, Int.max_value, Int.max_value), (0, 0, 0))
    ~f:(fun ((min_x, min_y, min_z), (max_x, max_y, max_z)) (x, y, z) ->
      ( (min min_x x, min min_y y, min min_z z),
        (max max_x x, max max_y y, max max_z z) ))

let in_bounds bounds (x, y, z) =
  let (min_x, min_y, min_z), (max_x, max_y, max_z) = bounds in
  x >= min_x - 1
  && x <= max_x + 1
  && y >= min_y - 1
  && y <= max_y + 1
  && z >= min_z - 1
  && z <= max_z + 1

let flood_fill coords =
  let bounds = bounds coords in
  let rec flood_fill' queue lava =
    match queue with
    | [] -> lava
    | (x, y, z) :: rest ->
        let empty_neighbors =
          neighbors (x, y, z)
          |> List.filter ~f:(fun neighbor ->
                 (not (Set.mem coords neighbor))
                 && in_bounds bounds neighbor
                 && not (Set.mem lava neighbor))
        in
        let new_exposed =
          List.fold empty_neighbors ~init:lava ~f:(fun acc neighbor ->
              Set.add acc neighbor)
        in
        flood_fill' (rest @ empty_neighbors) new_exposed
  in
  flood_fill' [ (0, 0, 0) ] (Set.empty (module Coord))

let part2 coords =
  let lava = flood_fill coords in
  Set.fold coords ~init:0 ~f:(fun acc coord ->
      let exposed_count =
        List.length (neighbors coord |> List.filter ~f:(Set.mem lava))
      in
      acc + exposed_count)

let () =
  let coords = parse_file "input.txt" in
  let part1_answer = part1 coords
  and part2_answer = part2 coords in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
