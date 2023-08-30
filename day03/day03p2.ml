open Core

let priority item_type =
  if Char.is_uppercase item_type then
    Char.to_int item_type - Char.to_int 'A' + 27
  else Char.to_int item_type - Char.to_int 'a' + 1

let prioritize_rucksack rucksack =
  List.map ~f:priority (String.to_list rucksack)
  |> List.sort ~compare:Int.compare

let split_into_groups lines =
  let rec aux acc group lines =
    match lines with
    | [] -> List.rev_append [ group ] acc
    | line :: rest ->
        if List.length group < 3 then aux acc (line :: group) rest
        else aux (List.rev group :: acc) [ line ] rest
  in
  aux [] [] lines

let prioritize_group group = List.map ~f:prioritize_rucksack group

let find_badge = function
  | [ first; second; third ] ->
      let rec find_match first second third =
        match (first, second, third) with
        | [], _, _ | _, [], _ | _, _, [] -> failwith "No match found"
        | x :: _, y :: _, z :: _ when x = y && x = z -> x
        | x :: xs, y :: ys, z :: zs ->
            if x <= y && x <= z then find_match xs second third
            else if y <= x && y <= z then find_match first ys third
            else find_match first second zs
      in
      find_match first second third
  | _ -> failwith "Invalid group"

let () =
  let total =
    In_channel.read_lines "input.txt"
    |> split_into_groups
    |> List.map ~f:prioritize_group
    |> List.sum (module Int) ~f:find_badge
  in
  printf "%d\n" total
