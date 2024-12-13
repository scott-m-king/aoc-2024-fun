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

let find_dir { l; x; y } : (int * int) option =
  match l with
  | '^' -> Some (x - 1, y)
  | '>' -> Some (x, y + 1)
  | 'v' -> Some (x + 1, y)
  | '<' -> Some (x, y - 1)
  | _ -> None

let get_next_pos grid ({ cell; steps } : steps) : steps option =
  match find_dir cell with
  | Some (x, y) ->
    Option.bind
      (get_cell grid (x, y))
      (function
        | { l = '#'; _ } -> Some (turn cell.l, (cell.x, cell.y))
        | _ -> Some (cell.l, (x, y)))
    |> Option.map (fun (l, (x, y)) -> Some { cell = { l; x; y }; steps = StepSet.add (x, y) steps })
    |> Option.value ~default:None
  | _ -> None

let get_next_hare hare grid : steps option =
  List.init 2 (fun x -> x)
  |> List.fold_left
       (fun next _ ->
         match next with
         | Some h -> get_next_pos grid h
         | _ -> None)
       (Some hare)

let rec walk_loop grid tortoise hare : bool =
  if tortoise.cell = hare.cell then
    true
  else
    find_next_loop grid tortoise hare

and find_next_loop grid tortoise hare : bool =
  let next_tortoise = get_next_pos grid tortoise in
  let next_hare = get_next_hare hare grid in
  match (next_tortoise, next_hare) with
  | Some t, Some h -> walk_loop grid t h
  | _ -> false

let try_at_position grid (x, y) (tortoise : steps) (hare : steps) =
  let char_at_pos = grid.(x).(y) in
  if (x = tortoise.cell.x && y = tortoise.cell.y) || char_at_pos = '#' || char_at_pos = '^' then
    0
  else (
    grid.(x).(y) <- '#';
    let result = walk_loop grid tortoise hare in
    grid.(x).(y) <- '.';
    if result then
      1
    else
      0
  )

let part2 input =
  let grid = parse_grid input in
  let indices = make_indices grid in
  let start = find_start grid indices in

  find_path grid indices |> StepSet.to_list
  |> List.fold_left
       (fun acc pos ->
         let t = { cell = start; steps = StepSet.of_list [ (start.x, start.y) ] } in
         let h = get_next_hare t grid |> Option.get in
         acc + try_at_position grid pos t h)
       0

let get_solution () = part2 (read_file "data/day-6.txt") |> print_int
