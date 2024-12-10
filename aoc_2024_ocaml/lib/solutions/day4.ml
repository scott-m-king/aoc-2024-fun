type cell = { l : char; x : int; y : int }

let print_cell_list_vertical cells =
  print_endline "Corners:";
  List.iteri
    (fun i cell -> Printf.printf "  %d: { letter: '%c'; x: %d; y: %d }\n" i cell.l cell.x cell.y)
    cells

let make_indices w h = List.init h (fun y -> List.init w (fun x -> (x, y))) |> List.flatten

let parse_input input =
  input |> String.split_on_char '\n'
  |> List.map (fun str -> String.to_seq str |> Array.of_seq)
  |> Array.of_list

let get_cell grid row col (i, j) =
  let x = row + i in
  let y = col + j in
  try Some { l = grid.(x).(y); x; y } with
  | Invalid_argument _ -> None

let rec find_xmas grid cell direction =
  match cell.l with
  | 'X' -> find_next grid 'M' cell direction
  | 'M' -> find_next grid 'A' cell direction
  | 'A' -> find_next grid 'S' cell direction
  | 'S' -> 1
  | _ -> 0

and find_next grid next_letter cell direction =
  match get_cell grid cell.x cell.y direction with
  | Some next_cell when next_cell.l = next_letter -> find_xmas grid next_cell direction
  | _ -> 0

let part1 input =
  let grid = parse_input input in

  make_indices (Array.length grid) (Array.length grid.(0))
  |> List.fold_left
       (fun acc (x, y) ->
         match grid.(x).(y) with
         | 'X' ->
           acc
           + ([ (0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1) ]
             |> List.map (fun dir -> find_xmas grid { l = 'X'; x; y } dir)
             |> List.fold_left ( + ) 0)
         | _ -> acc)
       0

let get_corners grid x y =
  [ (-1, -1); (1, -1); (-1, 1); (1, 1) ] (* nw sw ne se *)
  |> List.filter_map (fun dir -> get_cell grid x y dir)
  |> List.filter (fun x -> x.l = 'M' || x.l = 'S')

let part2 input =
  let grid = parse_input input in
  make_indices (Array.length grid) (Array.length grid.(0))
  |> List.fold_left
       (fun acc (x, y) ->
         match grid.(x).(y) with
         | 'A' ->
           let corners = get_corners grid x y in
           if List.length corners = 4 then
             acc
             +
             match corners with
             | [ nw; sw; ne; se ] when nw.l <> se.l && ne.l <> sw.l -> 1
             | _ -> 0
           else
             acc
         | _ -> acc)
       0

let get_solution () = part2 (Utils.read_file "data/day-4.txt") |> print_int
