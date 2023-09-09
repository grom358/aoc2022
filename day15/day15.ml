open Core
open Re

let parse_line line =
  let regex_pattern =
    Pcre.regexp
      "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), \
       y=(-?\\d+)"
  in
  match Pcre.extract ~rex:regex_pattern line with
  | [| _; sensor_x_str; sensor_y_str; beacon_x_str; beacon_y_str |] ->
      ( (Int.of_string sensor_x_str, Int.of_string sensor_y_str),
        (Int.of_string beacon_x_str, Int.of_string beacon_y_str) )
  | _ -> failwith "Invalid line"

let parse_file file_name =
  In_channel.read_lines file_name |> List.map ~f:parse_line

let manhattan_distance (x1, y1) (x2, y2) = Int.abs (x2 - x1) + Int.abs (y2 - y1)

(* Scanner range on given row *)
let scanner_range_on_row ((sensor_x, sensor_y), beacon) y =
  let radius = manhattan_distance (sensor_x, sensor_y) beacon in
  let min_y = sensor_y - radius and max_y = sensor_y + radius in
  if y < min_y || y > max_y then None
  else
    let max_x = radius - Int.abs (y - sensor_y) in
    Some (-max_x + sensor_x, max_x + sensor_x)

let range_overlap (a_start, a_end) (b_start, b_end) =
  (b_start >= a_start && b_start <= a_end)
  || (b_end >= a_start && b_end <= a_end)

(* Get the scanned ranges for a row *)
let scan_row sensor_beacon_list y =
  let rec aux sensor_beacon_list acc =
    match sensor_beacon_list with
    | [] -> acc
    | sensor_beacon :: rest -> (
        let range = scanner_range_on_row sensor_beacon y in
        match range with None -> aux rest acc | Some r -> aux rest (r :: acc))
  in
  let sorted_list =
    aux sensor_beacon_list []
    |> List.sort ~compare:(fun (x1, _) (x2, _) -> Int.compare x1 x2)
  in
  let rec merge = function
    | [] -> []
    | (curr_start, curr_end) :: rest ->
        (* Merge the range (curr_start, curr_end) with any overlapping range *)
        let overlapping, merged_tuple, remaining =
          List.fold rest
            ~init:(false, (curr_start, curr_end), [])
            ~f:(fun
                (overlap, (merge_min, merge_max), acc) (test_start, test_end) ->
              if range_overlap (merge_min, merge_max) (test_start, test_end)
              then
                (true, (min merge_min test_start, max merge_max test_end), acc)
              else
                (overlap, (merge_min, merge_max), (test_start, test_end) :: acc))
        in
        if overlapping then merge (merged_tuple :: remaining)
        else merged_tuple :: merge remaining
  in
  merge sorted_list

(* Count beacons in a row *)
let beacons_in_row sensor_beacon_list y =
  List.map sensor_beacon_list ~f:(fun (_, beacon) -> beacon)
  |> List.filter ~f:(fun (_, beacon_y) -> beacon_y = y)
  |> List.map ~f:(fun (beacon_x, _) -> beacon_x)
  |> List.dedup_and_sort ~compare:Int.compare
  |> List.length

let count_invalid_on_row sensor_beacon_list y =
  let beacons = beacons_in_row sensor_beacon_list y
  and count_scanned =
    scan_row sensor_beacon_list y
    |> List.sum
         (module Int)
         ~f:(fun (range_start, range_end) -> range_end - range_start + 1)
  in
  count_scanned - beacons

let part1 sensor_beacon_list = count_invalid_on_row sensor_beacon_list 2000000

let part2 sensor_beacon_list =
  let n = 4_000_000 in
  let found_position =
    let rec find_position row =
      if row >= n then None
      else
        let ranges = scan_row sensor_beacon_list row in
        if List.length ranges > 1 then
          let _, a = List.hd_exn ranges in
          Some (a + 1, row)
        else find_position (row + 1)
    in
    find_position 0
  in
  let x, y =
    match found_position with None -> failwith "Not found" | Some r -> r
  in
  (x * 4_000_000) + y

let () =
  let sensor_beacon_list = parse_file "input.txt" in
  let part1_answer = part1 sensor_beacon_list
  and part2_answer = part2 sensor_beacon_list in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
