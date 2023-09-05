open Core
open Re

type op_value = Int of int | Old
type op = Add of op_value | Multiple of op_value

let parse_op_value = function "old" -> Old | v -> Int (Int.of_string v)

type monkey = {
  items : int Queue.t;
  operation : op;
  test : int;
  on_true : int;
  on_false : int;
  mutable count : int;
}

let parse_monkey monkey =
  let regex_pattern =
    Pcre.regexp
      "Monkey \\d+:\\s+Starting items: (.*)\\s+Operation: new = old (.) \
       (\\d+|old)\\s+Test: divisible by (\\d+)\\s+If true: throw to monkey \
       (\\d+)\\s+If false: throw to monkey (\\d+)"
      ~flags:[ `MULTILINE ]
  in
  match Pcre.extract ~rex:regex_pattern monkey with
  | [| _; items_field; op_type; op_value_str; test; true_branch; false_branch |]
    ->
      let items =
        String.split ~on:',' items_field
        |> List.map ~f:(fun s -> String.strip s |> Int.of_string)
        |> Queue.of_list
      in
      let op_value = parse_op_value op_value_str in
      let operation =
        match op_type with
        | "*" -> Multiple op_value
        | "+" -> Add op_value
        | _ -> failwith "Invalid test operator"
      in
      {
        items;
        operation;
        test = Int.of_string test;
        on_true = Int.of_string true_branch;
        on_false = Int.of_string false_branch;
        count = 0;
      }
  | _ -> failwith "Invalid monkey"

let parse_file file_name =
  In_channel.read_all file_name
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:parse_monkey |> Array.of_list

let simulate_monkey monkey monkeys worry_calculator =
  while not (Queue.is_empty monkey.items) do
    let old = Queue.dequeue_exn monkey.items in
    let get_value = function Old -> old | Int x -> x in
    let new_value =
      match monkey.operation with
      | Add v -> old + get_value v
      | Multiple v -> old * get_value v
    in
    let worry = worry_calculator new_value in
    let throw_to =
      if Int.equal (worry mod monkey.test) 0 then monkey.on_true
      else monkey.on_false
    in
    monkey.count <- monkey.count + 1;
    Queue.enqueue monkeys.(throw_to).items worry
  done

let simulate_round monkeys worry_calculator =
  Array.iter monkeys ~f:(fun monkey ->
      simulate_monkey monkey monkeys worry_calculator)

let simulate_rounds monkeys worry_calculator n =
  for _ = 1 to n do
    simulate_round monkeys worry_calculator
  done

let _print_items monkeys =
  Array.iteri monkeys ~f:(fun i monkey ->
      printf "Monkey %d: %s\n" i
        (String.concat ~sep:", "
           (Queue.to_list monkey.items |> List.map ~f:Int.to_string)))

let _print_counts monkeys =
  Array.iteri
    ~f:(fun i monkey ->
      printf "Monkey %d inspected items %d times.\n" i monkey.count)
    monkeys

let monkey_business monkeys =
  let counts =
    Array.map ~f:(fun monkey -> monkey.count) monkeys
    |> Array.to_list
    |> List.sort ~compare:(fun a b -> Int.compare b a)
  in
  List.take counts 2 |> List.fold_left ~init:1 ~f:( * )

let part1 file_name =
  let worry_calculator x = x / 3 in
  let monkeys = parse_file file_name in
  simulate_rounds monkeys worry_calculator 20;
  monkey_business monkeys

let part2 file_name =
  let monkeys = parse_file file_name in
  let total_test =
    Array.fold monkeys ~init:1 ~f:(fun acc monkey -> acc * monkey.test)
  in
  let worry_calculator x = x mod total_test in
  simulate_rounds monkeys worry_calculator 10_000;
  monkey_business monkeys

let () =
  let file_name = "input.txt" in
  let part1_answer = part1 file_name in
  let part2_answer = part2 file_name in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
