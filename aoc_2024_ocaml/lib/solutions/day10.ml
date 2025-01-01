open Utils

module CellSet = struct
  type t = int_cell

  let compare = compare
end

module Visited = Set.Make (CellSet)

let directions = [ -1, 0 (* up *); 1, 0 (* down *); 0, -1 (* left *); 0, 1 (* right *) ]

let parse_input str =
  parse_grid str
  |> Array.map (fun row -> row |> Array.map (fun x -> int_of_char x - int_of_char '0'))
  |> fun grid -> grid, make_indices grid

let rec dfs grid curr_cell (f : int_cell -> 'a -> 'a) (start : 'a) : 'a =
  directions
  |> List.fold_left
       (fun acc (row, col) ->
         match get_int_cell grid (curr_cell.row + row, curr_cell.col + col) with
         | Some next_cell when next_cell.value = curr_cell.value + 1 ->
           (match next_cell.value with
            | 9 -> f next_cell acc
            | _ -> dfs grid next_cell f acc)
         | _ -> acc)
       start

let accumulate f (grid, indices) =
  indices
  |> List.fold_left
       (fun acc x ->
         match get_int_cell grid x with
         | Some cell when cell.value = 0 -> acc + f grid cell
         | _ -> acc)
       0

let part1 input =
  parse_input input
  |> accumulate (fun grid start ->
    Visited.cardinal (dfs grid start (fun set next -> Visited.add set next) Visited.empty))

let part2 input =
  parse_input input
  |> accumulate (fun grid start -> dfs grid start (fun _ acc -> acc + 1) 0)

let get_solution () = part2 (read_file "data/day-10.txt") |> print_int
