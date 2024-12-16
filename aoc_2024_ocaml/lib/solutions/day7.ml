open Utils

let parse_input str =
  let lines = str |> String.split_on_char '\n' in
  let mapping = List.map (fun line -> String.split_on_char ':' line) lines in
  mapping
  |> List.map (fun pair ->
         match pair with
         | sum :: nums ->
           let key = int_of_string sum in
           let value =
             String.split_on_char ' ' (List.hd nums) |> List.filter_map int_of_string_opt
           in
           (key, value)
         | _ -> (0, []))

let rec try_combos ?(acc = 0) (target, nums) : bool =
  let next =
    match nums with
    | x :: xs -> Some (x, xs)
    | _ -> None
  in

  match (acc, next) with
  | x, None when x = target -> true
  | x, Some (next_num, rest_nums) ->
    let add = try_combos ~acc:(x + next_num) (target, rest_nums) in
    let multiply = try_combos ~acc:(x * next_num) (target, rest_nums) in
    add || multiply
  | _ -> false

let part1 input =
  parse_input input |> List.filter try_combos
  |> List.fold_left (fun acc (target, _) -> acc + target) 0

let part2 _input = 0
let get_solution () = part1 (read_file "data/day-7.txt") |> print_int
