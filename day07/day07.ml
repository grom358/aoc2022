open Core

type directory = {
  _name : string;
  parent : directory option;
  directories : (string, directory) Hashtbl.t;
  mutable size : int;
}

let create_directory ?parent name =
  let directories = Hashtbl.create (module String) in
  { _name = name; parent; directories; size = 0 }

let add_file directory size =
  directory.size <- directory.size + size;
  let rec update_parent_sizes dir size_change =
    match dir.parent with
    | Some parent ->
        parent.size <- parent.size + size_change;
        update_parent_sizes parent size_change
    | None -> ()
  in
  update_parent_sizes directory size

let add_directory directory dirname =
  let new_directory = create_directory ~parent:directory dirname in
  Hashtbl.add_exn directory.directories ~key:dirname ~data:new_directory

let change_directory current_directory dirname =
  match dirname with
  | ".." -> (
      match current_directory.parent with
      | None -> failwith "Already at root directory"
      | Some dir -> dir)
  | _ -> Hashtbl.find_exn current_directory.directories dirname

let process_file_line line =
  let parts = String.split line ~on:' ' in
  match parts with
  | [ size_str; file_name ] ->
      let size = Int.of_string size_str in
      (size, file_name)
  | _ -> failwith "Invalid input format"

let starts_with str prefix =
  let prefix_length = String.length prefix in
  let str_length = String.length str in
  if prefix_length > str_length then false
  else String.equal (String.sub str ~pos:0 ~len:prefix_length) prefix

let read_file_by_lines filename ~process_line =
  In_channel.with_file filename ~f:(fun in_channel ->
      In_channel.iter_lines in_channel ~f:(fun line -> process_line line))

let make_parser root_directory =
  let working_directory = ref root_directory in
  fun line ->
    match line with
    | "$ cd /" -> working_directory := root_directory
    | line when starts_with line "$ cd " ->
        let dirname = String.sub line ~pos:5 ~len:(String.length line - 5) in
        working_directory := change_directory !working_directory dirname
    | line when starts_with line "dir " ->
        let dirname = String.sub line ~pos:4 ~len:(String.length line - 4) in
        add_directory !working_directory dirname
    | "$ ls" -> ()
    | line ->
        let size, _ = process_file_line line in
        add_file !working_directory size

let rec directory_sizes working_directory =
  let current_size = working_directory.size in
  let subdirectory_sizes =
    Hashtbl.fold working_directory.directories ~init:[]
      ~f:(fun ~key:_ ~data:subdir acc -> directory_sizes subdir @ acc)
  in
  current_size :: subdirectory_sizes

let part1 sizes =
  List.filter sizes ~f:(fun size -> size <= 100_000)
  |> List.sum (module Int) ~f:(fun size -> size)

let part2 sizes total_size =
  let total_disk_space = 70_000_000 in
  let required_space = 30_000_000 in
  let free_space = total_disk_space - total_size in
  let to_delete = required_space - free_space in
  let candidates = List.filter sizes ~f:(fun size -> size >= to_delete) in
  List.hd_exn (List.sort candidates ~compare:Int.compare)

let () =
  let root_directory = create_directory "/" in
  let parser = make_parser root_directory in
  read_file_by_lines "input.txt" ~process_line:parser;
  let dir_sizes = directory_sizes root_directory in
  let part1_answer = part1 dir_sizes in
  let part2_answer = part2 dir_sizes root_directory.size in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
