open Utils

let directions = [ 1, 0; 0, 1; -1, 0; 0, -1 ]

let parse_input str =
  let grid =
    parse_grid str
    |> Array.map (fun row -> row |> Array.map (fun x -> int_of_char x - int_of_char '0'))
  in
  let indices = make_indices grid in
  grid, indices

let rec dfs grid (cell : int_cell) =
  (* print_int_cell cell ; *)
  directions
  |> List.fold_left
       (fun acc (row, col) ->
         let next_pos = cell.row + row, cell.col + col in
         match get_int_cell grid next_pos with
         | Some next_pos when next_pos.i = cell.i + 1 ->
           if next_pos.i = 9 then 1 else acc + dfs grid next_pos
         | _ -> acc)
       0

(*h i*)
let part1 input =
  let grid, indices = parse_input input in
  print_char '\n' ;
  print_int_grid grid ;
  indices
  |> List.fold_left
       (fun acc x ->
         match get_int_cell grid x with
         | Some cell when cell.i = 0 ->
           let res = dfs grid cell in
           print_int_cell cell ;
           Printf.printf "res: %d\n" res ;
           acc + res
         | _ -> acc)
       0

let part2 _input = 0

let get_solution () = part1 (read_file "data/day-10-test-2.txt") |> print_int
