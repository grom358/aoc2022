open Core

type shape = Horizontal | Cross | BackwardsL | Vertical | Block

(* From bottom left corner the list of (x, y) offsets where shape has rock *)
let shape_offsets = function
  | Horizontal -> [ (0, 0); (1, 0); (2, 0); (3, 0) ]
  | Cross -> [ (1, 0); (0, 1); (1, 1); (2, 1); (1, 2) ]
  | BackwardsL -> [ (0, 0); (1, 0); (2, 0); (2, 1); (2, 2) ]
  | Vertical -> [ (0, 0); (0, 1); (0, 2); (0, 3) ]
  | Block -> [ (0, 0); (1, 0); (0, 1); (1, 1) ]

let shape_height = function
  | Horizontal -> 1
  | Cross -> 3
  | BackwardsL -> 3
  | Vertical -> 4
  | Block -> 2

type tile = Rock | Air

let tile_equal tile1 tile2 =
  match (tile1, tile2) with Rock, Rock -> true | Air, Air -> true | _ -> false

type jet = Left | Right
type falling_rock = { shape : shape; mutable bottom_left : int * int }

(* Used to track cycles *)
type cycle_state = { seen : int; fallen : int; height : int }

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let hash (key1, key2) = Hashtbl.hash key1 lxor Hashtbl.hash key2
end

type tower = {
  mutable height : int;
  (* Rocks that have fallen (ie. stopped due to colliding downwards) *)
  mutable fallen : int;
  (* Fallen rocks to simulate *)
  target : int;
  tiles : tile array array;
  jet_queue : jet Queue.t;
  shape_queue : shape Queue.t;
  mutable falling_rock : falling_rock;
  (* Jet pushes completed *)
  mutable jet_count : int;
  (* Height added to tower by cycle *)
  mutable added_by_repeats : int;
  (* Hashtable with keys (jet_index, shape_index)
   * where jet_index = jet_count % Queue.length jet_queue
   *       shape_index = fallen % Queue.length shape_queue
   *)
  seen : (IntPair.t, cycle_state) Hashtbl.t;
}

let cycle queue =
  let result = Queue.dequeue_exn queue in
  Queue.enqueue queue result;
  result

let create_tower jet_pattern target =
  let max_height = 10_000 in
  let tiles = Array.make_matrix ~dimx:max_height ~dimy:7 Air in
  let jet_queue = Queue.of_list jet_pattern
  and shape_queue =
    Queue.of_list [ Horizontal; Cross; BackwardsL; Vertical; Block ]
  in
  let first_shape = cycle shape_queue in
  let falling_rock = { shape = first_shape; bottom_left = (2, 3) } in
  {
    height = 0;
    fallen = 0;
    target;
    tiles;
    jet_queue;
    shape_queue;
    falling_rock;
    jet_count = 0;
    added_by_repeats = 0;
    seen = Hashtbl.create (module IntPair);
  }

let is_empty tower x y =
  x >= 0 && x <= 6 && y >= 0 && tile_equal tower.tiles.(y).(x) Air

let is_all_empty tower offsets x y =
  List.for_all offsets ~f:(fun (x_offset, y_offset) ->
      is_empty tower (x + x_offset) (y + y_offset))

let simulate_jet tower =
  let jet_direction = cycle tower.jet_queue
  and offsets = shape_offsets tower.falling_rock.shape
  and x, y = tower.falling_rock.bottom_left in
  let x' = match jet_direction with Left -> x - 1 | Right -> x + 1 in
  tower.jet_count <- tower.jet_count + 1;
  if is_all_empty tower offsets x' y then
    tower.falling_rock.bottom_left <- (x', y)

let detect_cycle tower =
  if tower.added_by_repeats = 0 then
    let jet_length = Queue.length tower.jet_queue
    and shape_length = Queue.length tower.shape_queue in
    let key = (tower.jet_count % jet_length, tower.fallen % shape_length) in
    let state = Hashtbl.find tower.seen key in
    match state with
    | Some state ->
        if state.seen = 2 then (
          (* On 3rd time seeing the same shape at same position in jet stream
           * we are in a cycle. Advance the simulation as close to target as
           * possible.
           *)
          let delta_height = tower.height - state.height
          and delta_fallen = tower.fallen - state.fallen in
          let repeats = (tower.target - tower.fallen) / delta_fallen in
          tower.added_by_repeats <- repeats * delta_height;
          tower.fallen <- tower.fallen + (repeats * delta_fallen));
        Hashtbl.set tower.seen ~key
          ~data:
            {
              seen = state.seen + 1;
              fallen = tower.fallen;
              height = tower.height;
            }
    | None ->
        Hashtbl.set tower.seen ~key
          ~data:{ seen = 1; fallen = tower.fallen; height = tower.height }

(* Simulate the rock falling. *)
let simulate_fall tower =
  let offsets = shape_offsets tower.falling_rock.shape
  and x, y = tower.falling_rock.bottom_left in
  let y' = y - 1 in
  if is_all_empty tower offsets x y' then
    tower.falling_rock.bottom_left <- (x, y')
  else (
    List.iter offsets ~f:(fun (x_offset, y_offset) ->
        tower.tiles.(y + y_offset).(x + x_offset) <- Rock);
    let add_height = shape_height tower.falling_rock.shape
    and next_shape = cycle tower.shape_queue in
    tower.fallen <- tower.fallen + 1;
    tower.height <- max tower.height (y + add_height);
    tower.falling_rock <-
      { shape = next_shape; bottom_left = (2, tower.height + 3) };
    detect_cycle tower)

let simulate_tick tower =
  simulate_jet tower;
  simulate_fall tower

let simulate_until_target tower =
  while tower.fallen < tower.target do
    simulate_tick tower
  done

let part1 jet_pattern =
  let tower = create_tower jet_pattern 2022 in
  simulate_until_target tower;
  tower.height + tower.added_by_repeats

let part2 jet_pattern =
  let tower = create_tower jet_pattern 1_000_000_000_000 in
  simulate_until_target tower;
  tower.height + tower.added_by_repeats

let () =
  let jet_pattern =
    In_channel.read_all "input.txt"
    |> String.strip |> String.to_list
    |> List.map ~f:(fun c ->
           match c with
           | '>' -> Right
           | '<' -> Left
           | _ -> failwith "Invalid jet")
  in
  let part1_answer = part1 jet_pattern in
  let part2_answer = part2 jet_pattern in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
