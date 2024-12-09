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

let part1 =
  let left, right = Utils.read_file "data/day1.txt" |> parse_input in

  let left_sorted, _right_sorted = (List.sort compare left, List.sort compare right) in

  List.combine left_sorted _right_sorted
  |> List.map (fun (x, y) -> abs (x - y))
  |> List.fold_left ( + ) 0

let part2 =
  let left, right = Utils.read_file "data/day1.txt" |> parse_input in
  left
  |> List.fold_left
       (fun acc curr ->
         let counts = count curr right in
         acc + (counts * curr))
       0

let get_solution () = Printf.printf "\n%d\n" part2
