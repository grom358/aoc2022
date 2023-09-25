open Core

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]

    let manhattan (x1, y1) (x2, y2) = Int.abs (x2 - x1) + Int.abs (y2 - y1)
  end

  include T
  include Comparator.Make (T)
end

type direction = Up | Right | Down | Left
type tile = Wall | Clear | Blizzard of direction

let parse_file file_name =
  let parse_line row line =
    String.to_list line
    |> List.mapi ~f:(fun col c ->
           let tile =
             match c with
             | '.' -> Clear
             | '#' -> Wall
             | '^' -> Blizzard Up
             | 'v' -> Blizzard Down
             | '<' -> Blizzard Left
             | '>' -> Blizzard Right
             | _ -> failwith "invalid input"
           in
           ((col, row), tile))
  in
  let lines = In_channel.read_lines file_name in
  let rows = List.length lines in
  let cols = String.length (List.hd_exn lines) in
  let map =
    lines
    |> List.foldi ~init:[] ~f:(fun row acc line -> acc @ parse_line row line)
  in
  (map, cols, rows)

let add_dir (x, y) direction =
  match direction with
  | Up -> (x, y - 1)
  | Right -> (x + 1, y)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)

let blizzard_maps map cols rows max_time =
  let blizzard_coords blizzards =
    List.map blizzards ~f:(fun (pos, _) -> pos) |> Set.of_list (module Coord)
  in
  let blizzards =
    List.filter_map map ~f:(fun (pos, tile) ->
        match tile with Wall | Clear -> None | Blizzard dir -> Some (pos, dir))
  in
  let coords = blizzard_coords blizzards in
  let rec aux blizzards acc max_time =
    if max_time = 0 then List.rev acc
    else
      let new_blizzards =
        List.map blizzards ~f:(fun (pos, dir) ->
            let new_pos = add_dir pos dir in
            let new_pos =
              match (new_pos, dir) with
              | (0, y), Left -> ((cols - 2, y), dir)
              | (x, y), Right when x = cols - 1 -> ((1, y), dir)
              | (x, 0), Up -> ((x, rows - 2), dir)
              | (x, y), Down when y = rows - 1 -> ((x, 1), dir)
              | (x, y), _ -> ((x, y), dir)
            in
            new_pos)
      in
      let coords = blizzard_coords new_blizzards in
      aux new_blizzards (coords :: acc) (max_time - 1)
  in
  aux blizzards [ coords ] max_time |> List.to_array

let neighbours coord cols rows =
  let directions = [ Up; Right; Down; Left ] in
  let rec aux acc = function
    | [] -> acc
    | dir :: rest ->
        let x, y = add_dir coord dir in
        if x >= 0 && x < cols && y >= 0 && y < rows then
          aux ((x, y) :: acc) rest
        else aux acc rest
  in
  aux [] directions

module Node = struct
  module T = struct
    type t = { cost : int; heuristic : int; pos : Coord.t } [@@deriving sexp_of]

    let compare a b =
      let a_cost = a.cost + a.heuristic and b_cost = b.cost + b.heuristic in
      let cmp_cost = Int.compare a_cost b_cost in
      if cmp_cost <> 0 then cmp_cost else Coord.compare a.pos b.pos
  end

  include T
  include Comparator.Make (T)
end

module SeenPos = struct
  module T = struct
    type t = Coord.t * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let shortest start goal start_time map_info =
  let cols, rows, walls, blizzard_maps, repeats_at = map_info in
  let pq = Pairing_heap.create ~cmp:Node.compare () in
  let seen = ref (Set.empty (module SeenPos)) in
  Pairing_heap.add pq
    {
      Node.cost = start_time;
      Node.heuristic = Coord.manhattan start goal;
      Node.pos = start;
    };
  seen := Set.add !seen (start, start_time);
  let rec loop () =
    match Pairing_heap.pop pq with
    | None -> failwith "No path"
    | Some node ->
        if Coord.compare node.pos goal = 0 then node.cost
        else
          let new_cost = node.cost + 1 in
          let blizzards = blizzard_maps.(new_cost % repeats_at) in
          let candidates =
            node.pos :: neighbours node.pos cols rows
            |> List.filter ~f:(fun coord ->
                   (not (Set.mem walls coord)) && not (Set.mem blizzards coord))
          in
          List.iter candidates ~f:(fun new_pos ->
              if not (Set.mem !seen (new_pos, new_cost)) then (
                seen := Set.add !seen (new_pos, new_cost);
                Pairing_heap.add pq
                  {
                    Node.cost = new_cost;
                    Node.heuristic = Coord.manhattan new_pos goal;
                    Node.pos = new_pos;
                  }));
          loop ()
  in
  loop ()

(* Greatest common divisor *)
let gcd a b =
  let rec euclidean a b = if b = 0 then a else euclidean b (a mod b) in
  if a >= 0 && b >= 0 then euclidean a b
  else if a < 0 then euclidean (-a) b
  else euclidean a (-b)

(* Least common multiple *)
let lcm a b = a * b / gcd a b

let () =
  let map, cols, rows = parse_file "input.txt" in
  let walls =
    List.filter_map map ~f:(fun (pos, tile) ->
        match tile with Wall -> Some pos | _ -> None)
    |> Set.of_list (module Coord)
  in
  let max_time = lcm (cols - 2) (rows - 2) in
  let blizzard_maps = blizzard_maps map cols rows max_time in
  let map_info = (cols, rows, walls, blizzard_maps, max_time) in
  let start = (1, 0) and goal = (cols - 2, rows - 1) in
  let there = shortest start goal 0 map_info in
  let back = shortest goal start there map_info in
  let part2_answer = shortest start goal back map_info in
  printf "Part 1: %d\nPart 2: %d\n" there part2_answer
