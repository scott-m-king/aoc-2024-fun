open Utils
open Core

let parse_input str =
  str
  |> String.split ~on:'\n'
  |> List.map ~f:(fun line -> String.split ~on:':' line)
  |> List.map ~f:(function
    | target :: nums :: _ ->
      let value = nums |> String.split ~on:' ' |> List.filter_map ~f:int_of_string_opt in
      int_of_string target, value
    | _ -> 0, [])

let rec try_combos acc (target, nums) ops : bool =
  let next =
    match nums with
    | x :: xs -> Some (x, xs)
    | _ -> None
  in
  match acc, next with
  | x, _ when x > target -> false
  | x, None when x = target -> true
  | x, Some (next_num, rest_nums) ->
    ops |> List.exists ~f:(fun fn -> try_combos (fn x next_num) (target, rest_nums) ops)
  | _ -> false

let part1 input =
  let ops = [ ( + ); ( * ) ] in
  parse_input input
  |> List.filter ~f:(fun x -> try_combos 0 x ops)
  |> List.fold ~init:0 ~f:(fun acc (target, _) -> acc + target)

let part2 input =
  let ops = [ ( + ); ( * ); (fun x y -> Printf.sprintf "%d%d" x y |> int_of_string) ] in
  parse_input input
  |> List.filter ~f:(fun x -> try_combos 0 x ops)
  |> List.fold ~init:0 ~f:(fun acc (target, _) -> acc + target)

let get_solution () = part2 (read_file "data/day-7.txt") |> eprintf "%d"
