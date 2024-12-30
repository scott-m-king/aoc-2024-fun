open Utils

module CellSet = struct
  type t = int_cell

  let compare = compare
end

module VisitedSet = Set.Make (CellSet)

let directions = [ 0, -1; 1, 0; 0, 1; -1, 0 ]

let parse_input str =
  parse_grid str
  |> Array.map (fun row -> row |> Array.map (fun x -> int_of_char x - int_of_char '0'))
  |> fun grid -> grid, make_indices grid

let rec dfs grid curr_cell visited =
  directions
  |> List.fold_left
       (fun acc (row, col) ->
         let next_pos = curr_cell.row + row, curr_cell.col + col in
         match get_int_cell grid next_pos with
         | Some next_cell when next_cell.value = curr_cell.value + 1 ->
           next_cell.value
           |> (function
            | 9 -> VisitedSet.add next_cell acc
            | _ -> dfs grid next_cell acc)
         | _ -> acc)
       visited

let part1 input =
  parse_input input
  |> fun (grid, indices) ->
  indices
  |> List.fold_left
       (fun acc x ->
         match get_int_cell grid x with
         | Some trailhead when trailhead.value = 0 ->
           acc + VisitedSet.cardinal (dfs grid trailhead VisitedSet.empty)
         | _ -> acc)
       0

let part2 _input = 0

let get_solution () = part1 (read_file "data/day-10.txt") |> print_int
