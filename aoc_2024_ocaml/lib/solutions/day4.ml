open Utils

let all_directions = [ 0, 1; 0, -1; 1, 0; -1, 0; 1, 1; -1, -1; 1, -1; -1, 1 ]

let corners = [ -1, -1; 1, -1; -1, 1; 1, 1 ] (* nw sw ne se *)

let rec find_xmas grid cell direction =
  match cell.l with
  | 'X' -> find_next grid 'M' cell direction
  | 'M' -> find_next grid 'A' cell direction
  | 'A' -> find_next grid 'S' cell direction
  | 'S' -> 1
  | _ -> 0

and find_next grid next_letter cell (i, j) =
  match get_cell grid (cell.x + i, cell.y + j) with
  | Some next_cell when next_cell.l = next_letter -> find_xmas grid next_cell (i, j)
  | _ -> 0

let get_corners grid x y =
  corners
  |> List.filter_map (fun (i, j) -> get_cell grid (x + i, y + j))
  |> List.filter (fun x -> x.l = 'M' || x.l = 'S')

let int_of_xmas_match grid x y =
  match get_corners grid x y with
  | [ nw; sw; ne; se ] when nw.l <> se.l && ne.l <> sw.l -> 1
  | _ -> 0

let part1 input =
  let grid = parse_grid input in
  make_indices grid
  |> List.fold_left
       (fun acc (x, y) ->
          match grid.(x).(y) with
          | 'X' ->
            acc
            + (all_directions
               |> List.map (fun dir -> find_xmas grid { l = 'X'; x; y } dir)
               |> List.fold_left ( + ) 0)
          | _ -> acc)
       0

let part2 input =
  let grid = parse_grid input in
  make_indices grid
  |> List.fold_left
       (fun acc (x, y) ->
          match grid.(x).(y) with
          | 'A' -> acc + int_of_xmas_match grid x y
          | _ -> acc)
       0

let get_solution () = part2 (read_file "data/day-4.txt") |> print_int
