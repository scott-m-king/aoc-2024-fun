open Utils

let up = (-1, 0)
let down = (1, 0)
let left = (0, -1)
let right = (0, 1)

module TupleSet = struct
  type t = int * int

  let compare = compare
end

module StepSet = Set.Make (TupleSet)

type steps = { cell : cell; steps : StepSet.t }
type steps_loop = { cell : cell; steps : StepSet.t; count : int }

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

let rec walk grid (steps : steps) =
  match steps.cell.l with
  | '^' -> find_next grid up steps
  | '>' -> find_next grid right steps
  | 'v' -> find_next grid down steps
  | '<' -> find_next grid left steps
  | _ -> steps

and find_next grid (x_pos, y_pos) { cell; steps } : steps =
  let next_x = cell.x + x_pos in
  let next_y = cell.y + y_pos in
  Option.bind
    (get_cell grid (next_x, next_y))
    (function
      | { l = '#'; _ } -> Some (turn cell.l, (cell.x, cell.y))
      | _ -> Some (cell.l, (next_x, next_y)))
  |> Option.map (fun (l, (x, y)) ->
         walk grid { cell = { l; x; y }; steps = StepSet.add (x, y) steps })
  |> Option.value ~default:{ cell; steps }

let find_path grid indices =
  find_start grid indices
  |> fun start ->
  walk grid { cell = start; steps = StepSet.of_list [ (start.x, start.y) ] }
  |> fun finish -> finish.steps

let part1 input =
  let grid = parse_grid input in
  let indices = make_indices grid in

  find_start grid indices
  |> fun start ->
  walk grid { cell = start; steps = StepSet.of_list [ (start.x, start.y) ] }
  |> fun finish -> StepSet.cardinal finish.steps

let clone_grid grid =
  let rows = Array.length grid in
  let cols =
    if rows > 0 then
      Array.length grid.(0)
    else
      0
  in
  Array.init rows (fun i -> Array.init cols (fun j -> grid.(i).(j)))

let rec walk_loop grid len steps : steps_loop =
  grid.(steps.cell.x).(steps.cell.y) <- 'X';
  if steps.count > len then
    { cell = steps.cell; steps = steps.steps; count = 0 }
  else
    match steps.cell.l with
    | '^' -> find_next_loop grid len up steps
    | '>' -> find_next_loop grid len right steps
    | 'v' -> find_next_loop grid len down steps
    | '<' -> find_next_loop grid len left steps
    | _ -> steps

and find_next_loop grid len (x_pos, y_pos) { cell; steps; count } : steps_loop =
  let next_x = cell.x + x_pos in
  let next_y = cell.y + y_pos in
  Option.bind
    (get_cell grid (next_x, next_y))
    (function
      | { l = '#'; _ }
      | { l = '0'; _ } ->
        Some (turn cell.l, (cell.x, cell.y))
      | _ -> Some (cell.l, (next_x, next_y)))
  |> Option.map (fun (l, (x, y)) ->
         walk_loop grid len
           { cell = { l; x; y }; steps = StepSet.add (x, y) steps; count = count + 1 })
  |> Option.value ~default:{ cell; steps; count }

let try_at_position grid (x, y) start len =
  let char_at_pos = grid.(x).(y) in
  if (x = start.x && y = start.y) || char_at_pos = '#' || char_at_pos = '^' then
    0
  else
    let temp_grid = clone_grid grid in
    temp_grid.(x).(y) <- '0';
    let result =
      walk_loop temp_grid len
        { cell = start; steps = StepSet.of_list [ (start.x, start.y) ]; count = 1 }
    in
    (* print_char '\n'; print_char_grid temp_grid; *)
    Printf.printf "Result: %d" result.count;
    (* print_char '\n'; *)
    if result.count = 0 then
      1
    else
      0

let part2 input =
  let grid = parse_grid input in
  let indices = make_indices grid in
  let start = find_start grid indices in
  let path = find_path grid indices |> StepSet.to_list in

  path
  |> List.fold_left (fun acc pos -> acc + try_at_position grid pos start (List.length indices)) 0

let get_solution () = part2 (read_file "data/day-6-test.txt") |> print_int
