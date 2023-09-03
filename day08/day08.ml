open Core

let char_to_int c =
  let char_code = Char.to_int c in
  let zero_char_code = Char.to_int '0' in
  char_code - zero_char_code

let parse_grid file_name =
  In_channel.read_lines file_name
  |> List.map ~f:(fun line -> String.to_array line |> Array.map ~f:char_to_int)
  |> List.to_array

let is_visible grid x y =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let max_tree_north x y =
    let max_value = ref (-1) in
    for y' = 0 to y - 1 do
      let h = grid.(y').(x) in
      if h > !max_value then max_value := h
    done;
    !max_value
  in
  let max_tree_south x y =
    let max_value = ref (-1) in
    for y' = y + 1 to num_rows - 1 do
      let h = grid.(y').(x) in
      if h > !max_value then max_value := h
    done;
    !max_value
  in
  let max_tree_west x y =
    let max_value = ref (-1) in
    for x' = 0 to x - 1 do
      let h = grid.(y).(x') in
      if h > !max_value then max_value := h
    done;
    !max_value
  in
  let max_tree_east x y =
    let max_value = ref (-1) in
    for x' = x + 1 to num_cols - 1 do
      let h = grid.(y).(x') in
      if h > !max_value then max_value := h
    done;
    !max_value
  in
  let h = grid.(y).(x) in
  h > max_tree_north x y
  || h > max_tree_east x y
  || h > max_tree_south x y
  || h > max_tree_west x y

let count_visible grid =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let count = ref 0 in
  for y = 0 to num_rows - 1 do
    for x = 0 to num_cols - 1 do
      if is_visible grid x y then count := !count + 1
    done
  done;
  !count

let scenic_score grid x y =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let h = grid.(y).(x) in
  let rec score_north y_ score =
    if y_ < 0 then score
    else
      let h_ = grid.(y_).(x) in
      if h_ >= h then score + 1 else score_north (y_ - 1) (score + 1)
  in
  let score_north = score_north (y - 1) 0 in
  let rec score_east x_ score =
    if x_ >= num_cols then score
    else
      let h_ = grid.(y).(x_) in
      if h_ >= h then score + 1 else score_east (x_ + 1) (score + 1)
  in
  let score_east = score_east (x + 1) 0 in
  let rec score_south y_ score =
    if y_ >= num_rows then score
    else
      let h_ = grid.(y_).(x) in
      if h_ >= h then score + 1 else score_south (y_ + 1) (score + 1)
  in
  let score_south = score_south (y + 1) 0 in
  let rec score_west x_ score =
    if x_ < 0 then score
    else
      let h_ = grid.(y).(x_) in
      if h_ >= h then score + 1 else score_west (x_ - 1) (score + 1)
  in
  let score_west = score_west (x - 1) 0 in
  score_north * score_east * score_south * score_west

let best_scenic_score grid =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let best_score = ref 0 in
  for y = 0 to num_rows - 1 do
    for x = 0 to num_cols - 1 do
      let score = scenic_score grid x y in
      if score > !best_score then best_score := score
    done
  done;
  !best_score

let () =
  let grid = parse_grid "input.txt" in
  let count = count_visible grid in
  let best_score = best_scenic_score grid in
  printf "Part 1: %d\nPart 2: %d\n" count best_score
