open Core

type instruction = Noop | Addx of int

let parse_line line =
  match String.split ~on:' ' line with
  | [ "noop" ] -> Noop
  | [ "addx"; n ] -> Addx (Int.of_string n)
  | _ -> failwith "Invalid instruction"

let parse_file file_name =
  In_channel.read_lines file_name |> List.map ~f:parse_line

let run_instructions instructions =
  let rec run_cpu x = function
    | [] -> []
    | Noop :: rest -> x :: run_cpu x rest
    | Addx n :: rest -> x :: x :: run_cpu (x + n) rest
  in
  1 :: run_cpu 1 instructions

let part1 results =
  List.filter_mapi results ~f:(fun i x ->
      if i mod 40 = 20 then Some (i * x) else None)
  |> List.sum (module Int) ~f:Fn.id

let part2 results =
  let render_line line =
    List.mapi line ~f:(fun i x -> if abs (i - x) <= 1 then "#" else ".")
    |> String.concat
  in
  let chunks =
    List.groupi (List.tl_exn results) ~break:(fun i _ _ -> i mod 40 = 0)
  in
  let lines = List.map chunks ~f:render_line in
  List.iter lines ~f:(printf "%s\n")

let () =
  let instructions = parse_file "input.txt" in
  let results = run_instructions instructions in
  let part1_answer = part1 results in
  printf "Part 1: %d\nPart 2:\n" part1_answer;
  part2 results
