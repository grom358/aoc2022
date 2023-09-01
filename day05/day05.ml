open Core

type move_command = { amount : int; src_idx : int; dst_idx : int }

let parse_move line =
  let regex =
    Str.regexp "move \\([0-9]+\\) from \\([0-9]+\\) to \\([0-9]+\\)"
  in
  if Str.string_match regex line 0 then
    {
      amount = int_of_string (Str.matched_group 1 line);
      src_idx = int_of_string (Str.matched_group 2 line) - 1;
      dst_idx = int_of_string (Str.matched_group 3 line) - 1;
    }
  else failwith "Invalid move command format"

let parse_config lines =
  List.drop_last_exn lines
  |> List.map ~f:String.to_array
  (* Each stack is 3 characters in width with each stack separated by space
   * Therefore every `i mod 4 = 1` element is the crate letter *)
  |> List.map ~f:(Array.filteri ~f:(fun i _ -> i mod 4 = 1))
  |> List.to_array
  (* Using the example input:
   *     [D]    
   * [N] [C]    
   * [Z] [M] [P]
   *  1   2   3
   * At this stage we now have:
   * [|[|' '; 'D'; ' '|];
   *   [|'N'; 'C'; ' '|];
   *   [|'Z'; 'M'; 'P'|]|]
   * Transpose will flip the matrix along the diagonal (that is switches
   * row and column indices):
   * [|[|' '; 'N'; 'Z'|];
   *   [|'D'; 'C'; 'M'|];
   *   [|' '; ' '; 'P'|]|]
   * so now each array element is a stack of crates.
   *)
  |> Array.transpose_exn
  |> Array.map ~f:Array.to_list
  |> Array.map ~f:(fun l -> List.drop_while l ~f:Char.is_whitespace)

let rec execute config instructions place =
  match instructions with
  | [] -> ()
  | { amount; src_idx; dst_idx } :: instructions ->
      let src = config.(src_idx) in
      let dst = config.(dst_idx) in
      let new_src = List.drop src amount in
      let new_dst = place (List.take src amount) dst in
      config.(src_idx) <- new_src;
      config.(dst_idx) <- new_dst;
      execute config instructions place

let calculate_top_crates config instructions place =
  execute config instructions place;
  Array.map ~f:List.hd_exn config |> Array.to_list |> String.of_char_list

let part1_place to_place dst = List.rev_append to_place dst
let part2_place to_place dst = to_place @ dst

let () =
  let config, instructions =
    match
      In_channel.read_all "input.txt"
      |> Str.split (Str.regexp "\n\n")
      |> List.map ~f:String.split_lines
    with
    | [ config_lines; instruction_lines ] ->
        (parse_config config_lines, List.map ~f:parse_move instruction_lines)
    | _ -> failwith "Invalid input file"
  in
  let config_part1 = Array.copy config in
  let top_crates_p1 =
    calculate_top_crates config_part1 instructions part1_place
  in
  let config_part2 = config in
  let top_crates_p2 =
    calculate_top_crates config_part2 instructions part2_place
  in
  printf "Part 1: %s\nPart 2: %s\n" top_crates_p1 top_crates_p2
