open Utils

let up = (-1, 0)
let down = (1, 0)
let left = (0, -1)
let right = (0, 1)

type steps = { cell : cell; steps : int }

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
  | c -> c

let get_next_cell cell next_x next_y steps = function
  | { l = '#'; _ } -> Some (turn cell.l, cell.x, cell.y, steps)
  | { l = 'X'; _ } -> Some (cell.l, next_x, next_y, steps)
  | { l = '.'; _ } -> Some (cell.l, next_x, next_y, steps + 1)
  | _ -> None

let rec walk grid (steps : steps) =
  grid.(steps.cell.x).(steps.cell.y) <- 'X';
  match steps.cell.l with
  | '^' -> find_next grid up steps
  | '>' -> find_next grid right steps
  | 'v' -> find_next grid down steps
  | '<' -> find_next grid left steps
  | _ -> steps

and find_next grid (x_pos, y_pos) { cell; steps } : steps =
  let next_x = cell.x + x_pos in
  let next_y = cell.y + y_pos in
  Option.bind (get_cell grid (next_x, next_y)) (get_next_cell cell next_x next_y steps)
  |> Option.map (fun (l, x, y, s) -> walk grid { cell = { l; x; y }; steps = s })
  |> Option.value ~default:{ cell; steps }

let part1 input =
  let grid = parse_grid input in
  let indices = make_indices grid in

  find_start grid indices
  |> fun start -> walk grid { cell = start; steps = 1 } |> fun finish -> finish.steps

let part2 _input = 0
let get_solution () = part1 (read_file "data/day-6.txt") |> print_int
