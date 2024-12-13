open Utils

module TupleSet = struct
  type t = int * int

  let compare = compare
end

module StepSet = Set.Make (TupleSet)

type steps = { cell : cell; steps : StepSet.t }

let steps_mapper steps =
 fun { l; x; y } -> Some { cell = { l; x; y }; steps = StepSet.add (x, y) steps.steps }

let cell_mapper = fun (cell : cell) -> Some cell

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

let find_dir { l; x; y } =
  match l with
  | '^' -> Some (x - 1, y)
  | '>' -> Some (x, y + 1)
  | 'v' -> Some (x + 1, y)
  | '<' -> Some (x, y - 1)
  | _ -> None

let get_next_pos (type a) grid cell mapper : a option =
  match find_dir cell with
  | Some (x, y) ->
    Option.bind
      (get_cell grid (x, y))
      (function
        | { l = '#'; _ } -> Some { l = turn cell.l; x = cell.x; y = cell.y }
        | _ -> Some { l = cell.l; x; y })
    |> Option.map mapper |> Option.value ~default:None
  | _ -> None

let rec walk grid steps =
  match get_next_pos grid steps.cell (steps_mapper steps) with
  | None -> steps
  | Some next -> walk grid next

let find_path grid indices =
  find_start grid indices
  |> fun start ->
  walk grid { cell = start; steps = StepSet.of_list [ (start.x, start.y) ] }
  |> fun finish -> finish.steps

let part1 input =
  let grid = parse_grid input in
  make_indices grid |> fun indices -> find_path grid indices |> StepSet.cardinal

let get_next_hare hare grid =
  List.init 2 (fun x -> x)
  |> List.fold_left
       (fun next _ ->
         match next with
         | Some h -> get_next_pos grid h cell_mapper
         | _ -> None)
       (Some hare)

let rec walk_loop grid tortoise hare =
  if tortoise = hare then true else find_next_loop grid tortoise hare

and find_next_loop grid tortoise hare =
  let next_tortoise = get_next_pos grid tortoise cell_mapper in
  let next_hare = get_next_hare hare grid in
  match (next_tortoise, next_hare) with
  | Some t, Some h -> walk_loop grid t h
  | _ -> false

let try_at_position grid (x, y) tortoise hare =
  let char_at_pos = grid.(x).(y) in
  match char_at_pos with
  | '^' -> 0
  | _ ->
    grid.(x).(y) <- '#';
    let result = walk_loop grid tortoise hare in
    grid.(x).(y) <- char_at_pos;
    if result then 1 else 0

let part2 input =
  let grid = parse_grid input in
  let indices = make_indices grid in
  let start = find_start grid indices in

  find_path grid indices |> StepSet.to_list
  |> List.fold_left
       (fun acc pos ->
         let h = get_next_hare start grid |> Option.get in
         acc + try_at_position grid pos start h)
       0

let get_solution () = part2 (read_file "data/day-6.txt") |> print_int
