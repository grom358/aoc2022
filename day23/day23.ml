open Core

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let hash (x, y) = Hashtbl.hash x lxor Hashtbl.hash y
end

let parse_file file_name =
  let parse_line row line =
    String.to_list line
    |> List.foldi ~init:[] ~f:(fun col acc c ->
           match c with
           | '.' -> acc
           | '#' -> (col, row) :: acc
           | _ -> failwith "invalid input")
  in
  In_channel.read_lines file_name
  |> List.concat_mapi ~f:parse_line
  |> Set.of_list (module Coord)

type direction =
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

let offset = function
  | North -> (0, -1)
  | NorthEast -> (1, -1)
  | East -> (1, 0)
  | SouthEast -> (1, 1)
  | South -> (0, 1)
  | SouthWest -> (-1, 1)
  | West -> (-1, 0)
  | NorthWest -> (-1, -1)

let neighbours coord =
  let neighbour (x, y) direction =
    let dx, dy = offset direction in
    (x + dx, y + dy)
  in
  [
    neighbour coord North;
    neighbour coord NorthEast;
    neighbour coord East;
    neighbour coord SouthEast;
    neighbour coord South;
    neighbour coord SouthWest;
    neighbour coord West;
    neighbour coord NorthWest;
  ]

let move (x, y) direction =
  let dx, dy = offset direction in
  (x + dx, y + dy)

let can_move neighbours direction =
  match neighbours with
  | [ n; ne; e; se; s; sw; w; nw ] -> (
      match direction with
      | North -> (not n) && (not ne) && not nw
      | South -> (not s) && (not se) && not sw
      | East -> (not e) && (not ne) && not se
      | West -> (not w) && (not nw) && not sw
      | _ -> false)
  | _ -> failwith "invalid neighbours"

let add_to_proposals proposals proposal elf =
  Hashtbl.update proposals proposal ~f:(function
    | None -> [ elf ]
    | Some elves -> elf :: elves)

let round elves directions =
  let proposals = Hashtbl.create (module Coord) in
  Set.iter elves ~f:(fun elf ->
      let neighbours = neighbours elf in
      let no_elves_nearby =
        List.for_all neighbours ~f:(fun neighbour ->
            not (Set.mem elves neighbour))
      in
      if not no_elves_nearby then
        let neighbours =
          List.map neighbours ~f:(fun neighbour -> Set.mem elves neighbour)
        in
        let proposed_dir =
          List.find directions ~f:(fun direction ->
              can_move neighbours direction)
        in
        match proposed_dir with
        | Some proposed_dir ->
            let proposal = move elf proposed_dir in
            add_to_proposals proposals proposal elf
        | _ -> ());
  Hashtbl.fold proposals ~init:elves
    ~f:(fun ~key:new_coord ~data:old_coords acc ->
      if List.length old_coords = 1 then
        let acc = Set.remove acc (List.hd_exn old_coords) in
        Set.add acc new_coord
      else acc)

let cycle directions = List.drop directions 1 @ List.take directions 1

let simulate elves rounds =
  let rec simulate' elves directions rounds =
    if rounds = 0 then elves
    else
      let new_elves = round elves directions in
      simulate' new_elves (cycle directions) (rounds - 1)
  in
  simulate' elves [ North; South; West; East ] rounds

let count_dirt elves min_x min_y max_x max_y =
  let count = ref 0 in
  for y = min_y to max_y do
    for x = min_x to max_x do
      if not (Set.mem elves (x, y)) then count := !count + 1
    done
  done;
  !count

let part1 elves =
  let round10 = simulate elves 10 in
  let min_x, min_y, max_x, max_y =
    (* calculate bounding box *)
    Set.fold round10 ~init:(Int.max_value, Int.max_value, 0, 0)
      ~f:(fun (min_x, min_y, max_x, max_y) (x, y) ->
        (min min_x x, min min_y y, max max_x x, max max_y y))
  in
  count_dirt round10 min_x min_y max_x max_y

let simulate_to_end elves =
  let rec simulate' elves directions round_num =
    let new_elves = round elves directions in
    if Set.equal elves new_elves then round_num
    else simulate' new_elves (cycle directions) (round_num + 1)
  in
  simulate' elves [ North; South; West; East ] 1

let () =
  let elves = parse_file "input.txt" in
  let part1_answer = part1 elves in
  let part2_answer = simulate_to_end elves in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
