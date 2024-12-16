open Utils

let parse_input str =
  str |> String.split_on_char '\n'
  |> List.map (fun line -> String.split_on_char ':' line)
  |> List.map (function
       | target :: nums :: _ ->
         let value = String.split_on_char ' ' nums |> List.filter_map int_of_string_opt in
         (int_of_string target, value)
       | _ -> (0, []))

let rec try_combos acc (target, nums) ops : bool =
  let next =
    match nums with
    | x :: xs -> Some (x, xs)
    | _ -> None
  in

  match (acc, next) with
  | x, _ when x > target -> false
  | x, None when x = target -> true
  | x, Some (next_num, rest_nums) ->
    ops |> List.exists (fun fn -> try_combos (fn x next_num) (target, rest_nums) ops)
  | _ -> false

let part1 input =
  let ops = [ ( + ); ( * ) ] in
  parse_input input
  |> List.filter (fun x -> try_combos 0 x ops)
  |> List.fold_left (fun acc (target, _) -> acc + target) 0

let part2 input =
  let ops = [ ( + ); ( * ); (fun x y -> Printf.sprintf "%d%d" x y |> int_of_string) ] in
  parse_input input
  |> List.filter (fun x -> try_combos 0 x ops)
  |> List.fold_left (fun acc (target, _) -> acc + target) 0

let get_solution () = part2 (read_file "data/day-7.txt") |> print_int
