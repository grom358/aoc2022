open Core

let parse_range range_str =
  match String.split ~on:'-' range_str with
  | [ start; end_ ] -> (Int.of_string start, Int.of_string end_)
  | _ -> failwith "Invalid range format"

let parse_line line =
  match String.split ~on:',' line with
  | [ range1; range2 ] ->
      let start1, end1 = parse_range range1 in
      let start2, end2 = parse_range range2 in
      (start1, end1, start2, end2)
  | _ -> failwith "Invalid line format"

let range_fully_contained (start1, end1, start2, end2) =
  (start1 >= start2 && end1 <= end2) || (start2 >= start1 && end2 <= end1)

let part1 lines =
  List.map ~f:parse_line lines
  |> List.filter ~f:range_fully_contained
  |> List.length

let is_range_overlap (start1, end1, start2, end2) =
  not (end1 < start2 || end2 < start1)

let part2 lines =
  List.map ~f:parse_line lines |> List.filter ~f:is_range_overlap |> List.length

let () =
  let lines = In_channel.read_lines "input.txt" in
  let part1_answer = part1 lines in
  let part2_answer = part2 lines in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
