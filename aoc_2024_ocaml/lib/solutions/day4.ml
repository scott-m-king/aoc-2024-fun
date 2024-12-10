type cell = { letter : char; x : int; y : int }

let make_indices arr = arr |> Array.mapi (fun x _ -> x)

let parse_input input =
  input |> String.split_on_char '\n'
  |> List.map (fun str -> String.to_seq str |> Array.of_seq)
  |> Array.of_list

let get_cell grid row col direction =
  match direction with
  | i, j -> (
    let x = row + i in
    let y = col + j in
    try Some { letter = grid.(x).(y); x; y } with
    | Invalid_argument _ -> None)

let rec find_xmas grid cell direction =
  match cell.letter with
  | 'X' -> find_next grid 'M' cell direction
  | 'M' -> find_next grid 'A' cell direction
  | 'A' -> find_next grid 'S' cell direction
  | 'S' -> 1
  | _ -> 0

and find_next grid next_letter cell direction =
  match get_cell grid cell.x cell.y direction with
  | Some next_cell when next_cell.letter = next_letter -> find_xmas grid next_cell direction
  | _ -> 0

let part1 input =
  let grid = parse_input input in

  let counter = ref 0 in

  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j cell ->
          if cell = 'X' then
            let found =
              [ (0, 1); (0, -1); (1, 0); (-1, 0); (1, 1); (-1, -1); (1, -1); (-1, 1) ]
              |> List.map (fun x -> find_xmas grid { letter = cell; x = i; y = j } x)
              |> List.fold_left ( + ) 0
            in
            counter := !counter + found)
        row)
    grid;

  !counter

let part2 _input = 0
let get_solution () = part1 (Utils.read_file "data/day-4.txt") |> print_int
