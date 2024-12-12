open Utils

let up = (-1, 0)
let down = (1, 0)
let left = (0, -0)
let right = (0, 1)

let find_start grid indices =
  let rec aux result positions =
    match positions with
    | pos :: rest -> (
      match get_cell grid pos with
      | Some cell when cell.l = '^' -> cell
      | _ -> aux result rest)
    | _ -> result
  in
  aux { l = grid.(0).(0); x = 0; y = 0 } indices

let turn char =
  match char with
  | '^' -> '>'
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | _ -> '.'

let rec walk grid cell =
  grid.(cell.x).(cell.y) <- 'X';
  match cell.l with
  | '^' -> find_next grid up cell
  | '>' -> find_next grid right cell
  | 'v' -> find_next grid down cell
  | '<' -> find_next grid left cell
  | _ -> None

and find_next grid (x_pos, y_pos) curr =
  print_cell curr;
  let next_x = curr.x + x_pos in
  let next_y = curr.y + y_pos in
  match get_cell grid (next_x, next_y) with
  | Some { l = '#'; _ } -> walk grid { l = turn curr.l; x = curr.x; y = curr.y }
  | Some { l = '.'; _ }
  | Some { l = 'X'; _ } ->
    walk grid { l = curr.l; x = next_x; y = next_y }
  | _ -> None

let part1 input =
  let grid = parse_grid input in
  let indices = make_indices grid in
  let start = find_start grid indices in
  let _finish = walk grid start in
  print_char_grid grid;

  Printf.printf "Start: (%d, %d)" start.x start.y;

  0

let part2 _input = 0
let get_solution () = part1 (read_file "data/day-6-test.txt") |> print_int
