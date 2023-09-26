open Core

let to_decimal snafu =
  let convert_digit = function
    | '=' -> -2
    | '-' -> -1
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | _ -> failwith "invalid snafu digit"
  in
  String.to_list snafu
  |> List.fold ~init:0 ~f:(fun decimal snafu_digit ->
         (decimal * 5) + convert_digit snafu_digit)

let parse_file file_name =
  In_channel.read_lines file_name |> List.map ~f:to_decimal

let to_snafu decimal =
  let rec aux decimal acc =
    if decimal = 0 then acc
    else
      let snafu_digit =
        match decimal % 5 with
        | 0 -> '0'
        | 1 -> '1'
        | 2 -> '2'
        | 3 -> '='
        | 4 -> '-'
        | _ -> failwith "unreachable"
      in
      let new_decimal = (decimal + 2) / 5 in
      aux new_decimal (snafu_digit :: acc)
  in
  aux decimal [] |> String.of_list

let () =
  let total = parse_file "input.txt" |> List.sum (module Int) ~f:Fn.id in
  let answer = to_snafu total in
  printf "%s\n" answer
