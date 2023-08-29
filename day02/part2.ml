open Core

type choice = Rock | Paper | Scissors

let score_choice = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let word_to_choice = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "Invalid choice"

type outcome = Win | Lose | Draw

let word_to_outcome = function
  | "X" -> Lose
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "Invalid outcome"

let score_outcome = function Win -> 6 | Draw -> 3 | Lose -> 0

let round_outcome = function
  | Rock, Rock | Scissors, Scissors | Paper, Paper -> Draw
  | Scissors, Rock | Paper, Scissors | Rock, Paper -> Win
  | _ -> Lose

let find_shape = function
  | opponent_choice, Draw -> opponent_choice
  | Rock, Win -> Paper
  | Rock, Lose -> Scissors
  | Scissors, Win -> Rock
  | Scissors, Lose -> Paper
  | Paper, Win -> Scissors
  | Paper, Lose -> Rock

let score_round (opponent_choice, my_choice) =
  score_outcome (round_outcome (opponent_choice, my_choice))
  + score_choice my_choice

let parse_line line =
  match String.split line ~on:' ' with
  | [ a; b ] ->
      let opponent_choice = word_to_choice a in
      let required_outcome = word_to_outcome b in
      let my_choice = find_shape (opponent_choice, required_outcome) in
      (opponent_choice, my_choice)
  | _ -> failwith "Invalid line"

let () =
  let total =
    In_channel.read_lines "input.txt"
    |> List.map ~f:parse_line
    |> List.sum (module Int) ~f:score_round
  in
  printf "%d\n" total
