open Core

let priority item_type =
  if Char.is_uppercase item_type then
    Char.to_int item_type - Char.to_int 'A' + 27
  else Char.to_int item_type - Char.to_int 'a' + 1

let prioritize compartment =
  List.map ~f:priority (String.to_list compartment)
  |> List.sort ~compare:Int.compare

let split rucksack =
  let length = String.length rucksack in
  let midpoint = length / 2 in
  let first_compartment = String.sub rucksack ~pos:0 ~len:midpoint in
  let second_compartment =
    String.sub rucksack ~pos:midpoint ~len:(length - midpoint)
  in
  (first_compartment, second_compartment)

let prioritize rucksack =
  let first_compartment, second_compartment = split rucksack in
  (prioritize first_compartment, prioritize second_compartment)

let find_error rucksack =
  let first_compartment, second_compartment = prioritize rucksack in
  let rec find_match first second =
    match (first, second) with
    | [], _ | _, [] -> failwith "No match in rucksack"
    | x :: xs, y :: ys ->
        if x = y then x
        else if y > x then
          find_match xs second (* Search next element from first *)
        else find_match first ys (* Continue search of first *)
  in
  find_match first_compartment second_compartment

let () =
  let total =
    In_channel.read_lines "input.txt" |> List.sum (module Int) ~f:find_error
  in
  printf "%d\n" total
