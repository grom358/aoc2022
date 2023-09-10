open Core
open Re

type room = { valve : string; flow_rate : int; to_tunnels : string list }

let parse_line line =
  let regex_pattern =
    Pcre.regexp
      "Valve (.*) has flow rate=(\\d+); tunnels? leads? to valves? (.*)"
  in
  match Pcre.extract ~rex:regex_pattern line with
  | [| _; valve; flow_rate_str; to_tunnels_str |] ->
      let to_tunnels =
        String.split ~on:',' to_tunnels_str |> List.map ~f:String.strip
      and flow_rate = Int.of_string flow_rate_str in
      { valve; flow_rate; to_tunnels }
  | _ -> failwith "Invalid line"

let floyd_warshall paths valves =
  let n = Map.length valves in
  let dist = Array.make_matrix ~dimx:n ~dimy:n 100 in
  for v = 0 to n - 1 do
    dist.(v).(v) <- 0
  done;
  let rec add_edges = function
    | [] -> ()
    | (src, dst) :: rest ->
        let u = Map.find_exn valves src and v = Map.find_exn valves dst in
        dist.(u).(v) <- 1;
        add_edges rest
  in
  add_edges paths;
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let x = dist.(i).(k) + dist.(k).(j) in
        if dist.(i).(j) > x then dist.(i).(j) <- x
      done
    done
  done;
  dist

type world = {
  relevant_rooms : int String.Map.t;
  valves : int String.Map.t;
  distances : int array array;
}

let parse_file file_name =
  let rooms = In_channel.read_lines file_name |> List.map ~f:parse_line in
  let valves =
    List.mapi ~f:(fun index room -> (room.valve, index)) rooms
    |> String.Map.of_alist_exn
  in
  let paths =
    List.fold ~init:[]
      ~f:(fun acc room ->
        let pairs =
          List.map ~f:(fun tunnel -> (room.valve, tunnel)) room.to_tunnels
        in
        acc @ pairs)
      rooms
  in
  let relevant_rooms =
    List.filter rooms ~f:(fun room -> room.flow_rate > 0)
    |> List.map ~f:(fun room -> (room.valve, room.flow_rate))
    |> String.Map.of_alist_exn
  in
  let distances = floyd_warshall paths valves in
  { relevant_rooms; valves; distances }

let distance world src dst =
  let u = Map.find_exn world.valves src and v = Map.find_exn world.valves dst in
  world.distances.(u).(v)

let create_adjaceny_map world =
  let relevant_rooms = Map.keys world.relevant_rooms in
  let edges =
    List.fold relevant_rooms ~init:[] ~f:(fun acc src ->
        let others =
          List.filter relevant_rooms ~f:(fun dst -> String.compare src dst <> 0)
        in
        let pairs =
          List.fold others ~init:[] ~f:(fun pair_acc dst ->
              let dist = distance world src dst in
              (src, dst, dist) :: pair_acc)
        in
        acc @ pairs)
    @ List.fold relevant_rooms ~init:[] ~f:(fun acc dst ->
          let dist = distance world "AA" dst in
          ("AA", dst, dist) :: acc)
  in
  List.fold_left edges ~init:String.Map.empty ~f:(fun acc (src, dest, weight) ->
      Map.update acc src ~f:(function
        | Some neighbors -> (dest, weight) :: neighbors
        | None -> [ (dest, weight) ]))

let find_paths world start_node max_time =
  let adjacency_map = create_adjaceny_map world in
  let rec dfs current_node volume time_remaining path visited_nodes
      visited_paths =
    match Map.find adjacency_map current_node with
    | Some neighbors ->
        let candidates =
          List.filter
            ~f:(fun (node, weight) ->
              (not (Set.mem visited_nodes node))
              && time_remaining - weight - 1 > 0)
            neighbors
        in
        if List.is_empty candidates then
          visited_paths := (path, volume) :: !visited_paths
        else
          List.iter
            ~f:(fun (neighbor, weight) ->
              let flow_rate = Map.find_exn world.relevant_rooms neighbor in
              let new_time_remaining = time_remaining - weight - 1 in
              let new_volume = volume + (new_time_remaining * flow_rate) in
              let new_path = neighbor :: path in
              let new_visited_nodes = Set.add visited_nodes neighbor in
              dfs neighbor new_volume new_time_remaining new_path
                new_visited_nodes visited_paths)
            candidates
    | None -> ()
  in
  let visited_paths = ref [] in
  let visited_nodes = String.Set.singleton start_node in
  dfs start_node 0 max_time [] visited_nodes visited_paths;
  !visited_paths

let part1 world =
  find_paths world "AA" 30
  |> List.sort ~compare:(fun (_, a) (_, b) -> Int.compare b a)
  |> List.hd_exn |> snd

let all_values_different a b =
  let s1 = String.Set.of_list a and s2 = String.Set.of_list b in
  Set.inter s1 s2 |> Set.is_empty

let part2 world =
  let candidates =
    find_paths world "AA" 26
    |> List.sort ~compare:(fun (p1, a) (p2, b) ->
           let cmp_vol = Int.compare b a in
           if cmp_vol <> 0 then cmp_vol else List.compare String.compare p1 p2)
  in
  let max = ref 0 in
  let rec check_candidates = function
    | [] -> ()
    | (elf, vol_a) :: rest ->
        List.iter rest ~f:(fun (elephant, vol_b) ->
            let total_flow = vol_a + vol_b in
            if total_flow > !max && all_values_different elf elephant then
              max := total_flow);
        check_candidates rest
  in
  check_candidates candidates;
  !max

let () =
  let world = parse_file "sample.txt" in
  let part1_answer = part1 world in
  let part2_answer = part2 world in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
