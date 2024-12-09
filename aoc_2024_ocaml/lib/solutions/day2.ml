let parse_input str =
  str |> String.split_on_char '\n'
  |> List.map (fun x ->
         x |> String.split_on_char ' '
         |> List.filter_map (fun y ->
                try Some (int_of_string y) with Failure _ -> None))

let rec is_sorted func = function
  | [] | [ _ ] -> true
  | x :: y :: xs -> func x y && is_sorted func (y :: xs)

let is_safe list =
  list |> Utils.windows 2
  |> List.for_all (fun lst ->
         match lst with
         | x :: y :: _ ->
             let diff = abs (x - y) in
             1 <= diff && 3 >= diff
         | _ -> false)

let part1 =
  Utils.read_file "data/day-2.txt"
  |> parse_input
  |> List.filter (fun lst ->
         (is_sorted ( > ) lst || is_sorted ( < ) lst) && is_safe lst)
  |> List.length

let get_solution () = part1 |> print_int
