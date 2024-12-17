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

let get_next_points (x1, y1) (x2, y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  [ x1 + dx, y1 + dy; x1 - dx, y1 - dy; x2 + dx, y2 + dy; x2 - dx, y2 - dy ]

let accumulate_points grid =
  fun key positions acc ->
  let next_points =
    get_combos positions
    |> List.map (fun (pos1, pos2) -> get_next_points pos1 pos2)
    |> List.flatten
    |> List.filter (fun pos ->
      get_cell grid pos
      |> function
      | Some { l; _ } when l <> key -> true
      | _ -> false)
  in
  acc @ next_points

let part1 input =
  let grid, coords = parse_input input in
  let positions = AntennaMap.fold (accumulate_points grid) coords [] in
  PairSet.of_list positions |> PairSet.cardinal

let part2 _input = 0

let get_solution () = part1 (read_file "data/day-8.txt") |> print_int
