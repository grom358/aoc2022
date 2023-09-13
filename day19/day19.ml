open Core
open Re

module Resources = struct
  type t = { ore : int; clay : int; obsidian : int }

  let zero = { ore = 0; clay = 0; obsidian = 0 }
  let one_ore = { ore = 1; clay = 0; obsidian = 0 }
  let one_clay = { ore = 0; clay = 1; obsidian = 0 }
  let one_obsidian = { ore = 0; clay = 0; obsidian = 1 }

  let add a b =
    {
      ore = a.ore + b.ore;
      clay = a.clay + b.clay;
      obsidian = a.obsidian + b.obsidian;
    }

  let checked_sub a b =
    let ore = a.ore - b.ore in
    let clay = a.clay - b.clay in
    let obsidian = a.obsidian - b.obsidian in
    if ore >= 0 && clay >= 0 && obsidian >= 0 then Some { ore; clay; obsidian }
    else None

  let mul a scalar =
    {
      ore = a.ore * scalar;
      clay = a.clay * scalar;
      obsidian = a.obsidian * scalar;
    }
end

type blueprint = {
  id : int;
  ore_robot_cost : Resources.t;
  clay_robot_cost : Resources.t;
  obsidian_robot_cost : Resources.t;
  geode_robot_cost : Resources.t;
}

let parse_line line =
  let regex_pattern =
    Pcre.regexp
      "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot \
       costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. \
       Each geode robot costs (\\d+) ore and (\\d+) obsidian."
  in
  match Pcre.extract ~rex:regex_pattern line with
  | [|
   _;
   id;
   ore_str;
   clay_str;
   obsidian_ore_str;
   obsidian_clay_str;
   geode_ore_str;
   geode_obsidian_str;
  |] ->
      {
        id = Int.of_string id;
        ore_robot_cost = { ore = Int.of_string ore_str; clay = 0; obsidian = 0 };
        clay_robot_cost =
          { ore = Int.of_string clay_str; clay = 0; obsidian = 0 };
        obsidian_robot_cost =
          {
            ore = Int.of_string obsidian_ore_str;
            clay = Int.of_string obsidian_clay_str;
            obsidian = 0;
          };
        geode_robot_cost =
          {
            ore = Int.of_string geode_ore_str;
            clay = 0;
            obsidian = Int.of_string geode_obsidian_str;
          };
      }
  | _ -> failwith "Invalid line"

let parse_file file_name =
  In_channel.read_lines file_name |> List.map ~f:parse_line

type factory = {
  blueprint : blueprint;
  max_ore_cost : int;
  minutes_remaining : int;
  geodes_secured : int;
  resources : Resources.t;
  resources_rate : Resources.t;
}

let create_factory blueprint minutes_remaining =
  let max_ore_cost =
    max blueprint.clay_robot_cost.ore
      (max blueprint.obsidian_robot_cost.ore blueprint.geode_robot_cost.ore)
  in
  {
    blueprint;
    max_ore_cost;
    minutes_remaining;
    geodes_secured = 0;
    resources = Resources.zero;
    resources_rate = Resources.one_ore;
  }

(* If robot can be chosen return the factory *)
let choose_robot factory cost robot robot_fn =
  let rec aux minutes_remaining minutes_passed =
    match minutes_remaining with
    | 0 -> None
    | _ -> (
        let resources =
          Resources.add factory.resources
            (Resources.mul factory.resources_rate minutes_passed)
        in
        match Resources.checked_sub resources cost with
        | Some resources ->
            let branch_factory =
              {
                factory with
                minutes_remaining;
                resources = Resources.add resources factory.resources_rate;
                resources_rate = Resources.add factory.resources_rate robot;
              }
            in
            let branch_factory = robot_fn branch_factory in
            Some branch_factory
        | None -> aux (minutes_remaining - 1) (minutes_passed + 1))
  in
  aux (factory.minutes_remaining - 1) 0

let branch factory =
  let viable_robots =
    [
      ( factory.resources_rate.ore < factory.max_ore_cost,
        factory.blueprint.ore_robot_cost,
        Resources.one_ore,
        Fn.id );
      ( factory.resources_rate.clay < factory.blueprint.obsidian_robot_cost.clay,
        factory.blueprint.clay_robot_cost,
        Resources.one_clay,
        Fn.id );
      ( factory.resources_rate.obsidian
        < factory.blueprint.geode_robot_cost.obsidian
        && factory.resources_rate.clay > 0,
        factory.blueprint.obsidian_robot_cost,
        Resources.one_obsidian,
        Fn.id );
      ( factory.resources_rate.obsidian > 0,
        factory.blueprint.geode_robot_cost,
        Resources.zero,
        fun f ->
          { f with geodes_secured = f.geodes_secured + f.minutes_remaining } );
    ]
  in
  let options =
    List.fold viable_robots ~init:[]
      ~f:(fun acc (viable, cost, robot, robot_fn) ->
        if viable then choose_robot factory cost robot robot_fn :: acc else acc)
  in
  List.filter_opt options

(* The upper bound of geodes can produce. Assume unlimited ore and clay *)
let bound factory =
  let geode_cost = factory.blueprint.geode_robot_cost.obsidian in
  let rec aux obsidian obsidian_rate geodes_passed minutes_remaining =
    if minutes_remaining = 0 then geodes_passed
    else if obsidian >= geode_cost then
      let obsidian = obsidian + obsidian_rate - geode_cost in
      aux obsidian obsidian_rate
        (geodes_passed + minutes_remaining)
        (minutes_remaining - 1)
    else
      let obsidian = obsidian + obsidian_rate in
      let obsidian_rate = obsidian_rate + 1 in
      aux obsidian obsidian_rate geodes_passed (minutes_remaining - 1)
  in
  aux factory.resources.obsidian factory.resources_rate.obsidian
    factory.geodes_secured
    (factory.minutes_remaining - 1)

let branch_and_bound factory =
  let rec branch_and_bound' factories best =
    match factories with
    | [] -> best
    | factory :: rest ->
        let best = max factory.geodes_secured best in
        let branches =
          branch factory |> List.filter ~f:(fun branch -> bound branch > best)
        in
        branch_and_bound' (rest @ branches) best
  in
  branch_and_bound' [ factory ] 0

let part1 blueprints =
  List.fold blueprints ~init:0 ~f:(fun acc blueprint ->
      let best = branch_and_bound (create_factory blueprint 24) in
      acc + (blueprint.id * best))

let part2 blueprints =
  List.take blueprints 3
  |> List.fold ~init:1 ~f:(fun acc blueprint ->
         let best = branch_and_bound (create_factory blueprint 32) in
         acc * best)

let () =
  let blueprints = parse_file "input.txt" in
  let part1_answer = part1 blueprints in
  let part2_answer = part2 blueprints in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
