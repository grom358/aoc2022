open Core

type operator = Add | Subtract | Multiply | Divide
type operation = Number of int | Calculate of operator * string * string

let parse_line line =
  let parts = String.split ~on:':' line |> List.map ~f:String.strip in
  match parts with
  | [ name; operation ] ->
      let words = String.split ~on:' ' operation in
      let op =
        match words with
        | [ lhs; "+"; rhs ] -> Calculate (Add, lhs, rhs)
        | [ lhs; "-"; rhs ] -> Calculate (Subtract, lhs, rhs)
        | [ lhs; "*"; rhs ] -> Calculate (Multiply, lhs, rhs)
        | [ lhs; "/"; rhs ] -> Calculate (Divide, lhs, rhs)
        | [ num ] -> Number (Int.of_string num)
        | _ -> failwith "invalid operation"
      in
      (name, op)
  | _ -> failwith "invalid line"

let parse_file file_name =
  In_channel.read_lines file_name
  |> List.map ~f:parse_line |> String.Map.of_alist_exn

let rec calculate_monkey monkeys name =
  let monkey = Map.find_exn monkeys name in
  match monkey with
  | Number num -> num
  | Calculate (op, lhs, rhs) -> (
      let a = calculate_monkey monkeys lhs
      and b = calculate_monkey monkeys rhs in
      match op with
      | Add -> a + b
      | Subtract -> a - b
      | Multiply -> a * b
      | Divide -> a / b)

let rec depends_on_human monkeys name =
  if String.equal name "humn" then true
  else
    let monkey = Map.find_exn monkeys name in
    match monkey with
    | Number _ -> false
    | Calculate (_, lhs, rhs) ->
        depends_on_human monkeys lhs || depends_on_human monkeys rhs

let rec calculate_human monkeys name value =
  if String.equal name "humn" then value
  else
    match Map.find_exn monkeys name with
    | Number n -> n
    | Calculate (op, lhs, rhs) ->
        let new_name, new_value =
          if depends_on_human monkeys lhs then
            let rhs_num = calculate_monkey monkeys rhs in
            let new_value =
              match op with
              | Add -> value - rhs_num
              | Subtract -> value + rhs_num
              | Multiply -> value / rhs_num
              | Divide -> value * rhs_num
            in
            (lhs, new_value)
          else
            let lhs_num = calculate_monkey monkeys lhs in
            let new_value =
              match op with
              | Add -> value - lhs_num
              | Subtract -> lhs_num - value
              | Multiply -> value / lhs_num
              | Divide -> lhs_num / value
            in
            (rhs, new_value)
        in
        calculate_human monkeys new_name new_value

let part2 monkeys =
  let root = Map.find_exn monkeys "root" in
  match root with
  | Number _ -> failwith "root monkey must be calculated"
  | Calculate (_, lhs, rhs) ->
      let name, value =
        if depends_on_human monkeys lhs then
          let rhs_num = calculate_monkey monkeys rhs in
          (lhs, rhs_num)
        else
          let lhs_num = calculate_monkey monkeys lhs in
          (rhs, lhs_num)
      in
      calculate_human monkeys name value

let () =
  let monkeys = parse_file "input.txt" in
  let part1_answer = calculate_monkey monkeys "root" in
  let part2_answer = part2 monkeys in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
