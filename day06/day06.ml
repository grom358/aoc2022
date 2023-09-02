open Core

type letterset = int

let create_letterset () : letterset = 0
let letterset_index letter = Char.to_int letter - Char.to_int 'a'

let set_letter letterset letter =
  let index = letterset_index letter in
  letterset lor (1 lsl index)

let has_letter letterset letter =
  let index = letterset_index letter in
  letterset land (1 lsl index) <> 0

let is_marker list size =
  let rec count_unique list acc seen size =
    if acc < size then
      match list with
      | [] -> acc
      | hd :: tl ->
          if has_letter seen hd then acc
          else
            let new_seen = set_letter seen hd in
            count_unique tl (acc + 1) new_seen size
    else acc
  in
  let count = count_unique list 0 (create_letterset ()) size in
  count >= size

let find_marker list size =
  let rec find_marker_helper list acc =
    if is_marker list size then acc + size
    else find_marker_helper (List.drop list 1) (acc + 1)
  in
  find_marker_helper list 0

let () =
  let input = String.to_list (In_channel.read_all "input.txt") in
  let part1_answer = find_marker input 4 in
  let part2_answer = find_marker input 14 in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
