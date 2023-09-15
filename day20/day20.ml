open Core

module EncryptedFile = struct
  type node = {
    value : int;
    mutable prev : node option;
    mutable next : node option;
  }

  type t = { zero : node; length : int; nodes : node list }

  let of_list list =
    let nodes =
      List.fold list ~init:[] ~f:(fun acc value ->
          { value; prev = None; next = None } :: acc)
      |> List.rev
    in
    let first = List.hd_exn nodes in
    let zero = ref first in
    let last = List.last_exn nodes in
    let rec link nodes prev =
      match nodes with
      | [] -> failwith "unreachable"
      | tl :: [] ->
          if tl.value = 0 then zero := tl;
          tl.prev <- Some prev;
          tl.next <- Some first
      | hd :: next :: rest ->
          if hd.value = 0 then zero := hd;
          hd.prev <- Some prev;
          hd.next <- Some next;
          link (next :: rest) hd
    in
    link nodes last;
    { zero = !zero; length = List.length nodes; nodes }

  let prev_exn node =
    match node.prev with Some n -> n | None -> failwith "no prev"

  let next_exn node =
    match node.next with Some n -> n | None -> failwith "no next"

  let mix encrypted_file =
    let insert node prev next =
      next.prev <- Some node;
      node.next <- Some next;
      prev.next <- Some node;
      node.prev <- Some prev
    in
    List.iter encrypted_file.nodes ~f:(fun node ->
        let move = node.value % (encrypted_file.length - 1) in
        if move <> 0 then (
          (next_exn node).prev <- node.prev;
          (prev_exn node).next <- node.next;
          let head = ref node in
          if move > 0 then (
            for _ = move downto 1 do
              head := next_exn !head
            done;
            let prev = !head in
            let next = next_exn !head in
            insert node prev next)
          else (
            for _ = move to -1 do
              head := prev_exn !head
            done;
            let prev = prev_exn !head in
            let next = !head in
            insert node prev next)))

  let decrypt encrypted_file =
    let grove encrypted_file nth =
      let head = ref encrypted_file.zero in
      for _ = 1 to nth do
        head := next_exn !head
      done;
      !head.value
    in
    let length = encrypted_file.length in
    let grove_coordinates =
      [ 1000; 2000; 3000 ] |> List.map ~f:(fun coord -> coord % length)
    in
    List.sum
      (module Int)
      grove_coordinates
      ~f:(fun coord -> grove encrypted_file coord)

  let read_file file_name decryption_key =
    In_channel.read_lines file_name
    |> List.map ~f:(fun s -> Int.of_string s * decryption_key)
    |> of_list
end

let part1 file_name =
  let encrypted = EncryptedFile.read_file file_name 1 in
  EncryptedFile.mix encrypted;
  EncryptedFile.decrypt encrypted

let part2 file_name =
  let encrypted = EncryptedFile.read_file file_name 811589153 in
  for _ = 1 to 10 do
    EncryptedFile.mix encrypted
  done;
  EncryptedFile.decrypt encrypted

let () =
  let file_name = "input.txt" in
  let part1_answer = part1 file_name in
  let part2_answer = part2 file_name in
  printf "Part 1: %d\nPart 2: %d\n" part1_answer part2_answer
