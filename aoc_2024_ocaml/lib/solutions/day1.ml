let parse_input str =
  str |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         let nums =
           String.split_on_char ' ' line
           |> List.filter_map (fun s ->
                  try Some (int_of_string s) with
                  | _ -> None)
         in
         match nums with
         | first :: second :: _ -> Some (first, second)
         | _ -> None)
  |> List.split

let count target list = list |> List.filter (fun x -> x = target) |> List.length

let part1 input =
  let left, right = parse_input input in

  List.combine (List.sort compare left) (List.sort compare right)
  |> List.map (fun (x, y) -> abs (x - y))
  |> List.fold_left ( + ) 0

let part2 input =
  let left, right = parse_input input in
  left
  |> List.fold_left
       (fun acc curr ->
         let counts = count curr right in
         acc + (counts * curr))
       0

let get_solution () = part2 (Utils.read_file "data/day-1.txt") |> print_int
