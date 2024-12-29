open Utils
module AntennaMap = Map.Make (Char)

type pos = int * int

module Pair = struct
  type t = pos

  let compare = compare
end

module PairSet = Set.Make (Pair)

let add_or_default key pos map =
  match AntennaMap.find_opt key map with
  | Some lst -> AntennaMap.add key ([ pos ] @ lst) map
  | None -> AntennaMap.add key [ pos ] map

let parse_input str =
  let grid, indices = parse_grid_with_indices str in
  let map =
    indices
    |> List.fold_left
         (fun acc (x, y) ->
            match grid.(x).(y) with
            | '.' -> acc
            | c -> add_or_default c (x, y) acc)
         AntennaMap.empty
  in
  grid, map

let get_combos positions =
  positions
  |> List.mapi (fun i x ->
    positions |> List.filteri (fun j _ -> j > i) |> List.map (fun y -> x, y))
  |> List.concat

let is_valid_pos grid (x, y) (x1, y1) (x2, y2) =
  try
    let within_bounds =
      match get_cell grid (x, y) with
      | Some _ -> true
      | None -> false
    in
    let is_valid = x <> x1 && x <> x2 && y <> y1 && y <> y2 in
    within_bounds && is_valid
  with
  | _ -> false

let get_next_points grid (x1, y1) (x2, y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  [ x1 + dx, y1 + dy; x1 - dx, y1 - dy; x2 + dx, y2 + dy; x2 - dx, y2 - dy ]
  |> List.filter (fun pos -> is_valid_pos grid pos (x1, y1) (x2, y2))

let part1 input =
  let grid, coords = parse_input input in
  let positions =
    AntennaMap.fold
      (fun _key positions acc ->
         let next_points =
           get_combos positions
           |> List.map (fun (pos1, pos2) -> get_next_points grid pos1 pos2)
           |> List.flatten
         in
         acc @ next_points)
      coords
      []
  in
  PairSet.of_list positions |> PairSet.cardinal

let rec find_gcd a b =
  match b with
  | 0 -> a
  | _ -> find_gcd b (a mod b)

let rec find_line_points grid (x, y) (step_x, step_y) acc =
  match get_cell grid (x, y) with
  | None -> acc
  | Some _ ->
    find_line_points grid (x + step_x, y + step_y) (step_x, step_y) ((x, y) :: acc)

let get_all_next_points grid (x1, y1) (x2, y2) =
  let dx, dy = x2 - x1, y2 - y1 in
  let gcd = find_gcd dx dy in
  let step_x, step_y = dx / gcd, dy / gcd in
  find_line_points grid (x1, y1) (step_x, step_y) []
  @ find_line_points grid (x1, y1) (-step_x, -step_y) []

let part2 input =
  let grid, coords = parse_input input in
  let positions =
    AntennaMap.fold
      (fun _key positions acc ->
         let next_points =
           get_combos positions
           |> List.map (fun (pos1, pos2) -> get_all_next_points grid pos1 pos2)
           |> List.flatten
         in
         acc @ next_points)
      coords
      []
  in
  PairSet.of_list positions |> PairSet.cardinal

let get_solution () = part2 (read_file "data/day-8.txt") |> print_int
