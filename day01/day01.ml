open Core

let take_top_n list n =
  let insert_sorted_with_limit integer sorted_list limit =
    let rec insert_rec lst =
      match lst with
      | [] -> [ integer ]
      | hd :: tl as rest ->
          if integer >= hd then integer :: rest else hd :: insert_rec tl
    in
    let new_sorted_list = insert_rec sorted_list in
    List.take new_sorted_list limit
  in
  List.fold_left list ~init:[] ~f:(fun acc number ->
      insert_sorted_with_limit number acc n)

let find_top_elves input n =
  let groups =
    Str.split (Str.regexp "\n\n") input
    |> List.map ~f:String.split_lines
    |> List.map ~f:(List.sum (module Int) ~f:Int.of_string)
  in
  take_top_n groups n |> List.sum (module Int) ~f:Fn.id

let () =
  let input = In_channel.read_all "input.txt" in
  let total = find_top_elves input 3 in
  printf "Sum of top elves: %d\n" total
