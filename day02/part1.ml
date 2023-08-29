open Core

type choice = Rock | Paper | Scissors

let score_choice = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let word_to_choice = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> failwith "Invalid choice"

type outcome = Win | Loss | Draw

let score_outcome = function Win -> 6 | Draw -> 3 | Loss -> 0

let round_outcome = function
  | Rock, Rock | Scissors, Scissors | Paper, Paper -> Draw
  | Scissors, Rock | Paper, Scissors | Rock, Paper -> Win
  | _ -> Loss

let score_round (opponent_choice, my_choice) =
  score_outcome (round_outcome (opponent_choice, my_choice))
  + score_choice my_choice

let parse_line line =
  match String.split line ~on:' ' with
  | [ a; b ] -> (word_to_choice a, word_to_choice b)
  | _ -> failwith "Invalid line"

let () =
  let total =
    In_channel.read_lines "input.txt"
    |> List.map ~f:parse_line
    |> List.sum (module Int) ~f:score_round
  in
  printf "%d\n" total
