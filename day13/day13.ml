open Core

type item = Number of int | NumberList of item list

let parse_number list = List.split_while ~f:Char.is_digit list

let parse_list list =
  let rec parse_inner list acc =
    match list with
    | [] -> (acc, [])
    | hd :: tl -> (
        match hd with
        | '[' ->
            let inner_list, rest = parse_inner tl [] in
            parse_inner rest (NumberList inner_list :: acc)
        | ']' -> (List.rev acc, tl)
        | ',' -> parse_inner tl acc
        | c when Char.is_digit c ->
            let number_list, rest = parse_number list in
            let number = String.of_list number_list |> Int.of_string in
            parse_inner rest (Number number :: acc)
        | x -> failwith ("Invalid input " ^ Char.to_string x))
  in
  let result, _ = parse_inner list [] in
  List.hd_exn result

let parse_line line =
  let chars = String.to_list line in
  match chars with
  | hd :: _ when Char.compare hd '[' = 0 -> parse_list chars
  | _ -> failwith "Invalid line"

let parse_file file_name =
  In_channel.read_all file_name
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun lines ->
         let lines = String.split_lines lines |> List.map ~f:parse_line in
         match lines with [ x; y ] -> (x, y) | _ -> failwith "Invalid pair")

let rec compare a b =
  match (a, b) with
  | Number a, Number b -> Int.compare a b
  | NumberList a, NumberList b -> List.compare compare a b
  | NumberList a, Number b -> compare (NumberList a) (NumberList [ Number b ])
  | Number a, NumberList b -> compare (NumberList [ Number a ]) (NumberList b)

let part1 pairs =
  List.mapi pairs ~f:(fun i (a, b) -> if compare a b <= 0 then i + 1 else 0)
  |> List.sum (module Int) ~f:Fn.id

let find_packet packets target =
  match List.findi packets ~f:(fun _ x -> compare x target = 0) with
  | Some (index, _) -> index + 1
  | None -> failwith "Packet not found"

let part2 pairs =
  let orginal_packets = List.concat_map ~f:(fun (x, y) -> [ x; y ]) pairs
  and divider1 = parse_line "[[2]]"
  and divider2 = parse_line "[[6]]" in
  let packets = List.sort ~compare (divider1 :: divider2 :: orginal_packets) in
  let divider1_index = find_packet packets divider1
  and divider2_index = find_packet packets divider2 in
  divider1_index * divider2_index

let () =
  let pairs = parse_file "input.txt" in
  let part1_answer = part1 pairs and part2_answer = part2 pairs in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
