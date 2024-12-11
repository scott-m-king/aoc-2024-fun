open Utils

let parse_input str =
  str |> String.split_on_char '\n'
  |> List.map (fun x -> x |> String.split_on_char ' ' |> List.map int_of_string)

let rec is_sorted cmp = function
  | x :: y :: xs -> cmp x y && is_sorted cmp (y :: xs)
  | _ -> true

let is_safe list =
  list |> windows 2
  |> List.for_all (fun lst ->
         match lst with
         | x :: y :: _ ->
           let diff = abs (x - y) in
           1 <= diff && 3 >= diff
         | _ -> false)

let part1 input =
  input |> parse_input
  |> List.filter (fun lst -> (is_sorted ( > ) lst || is_sorted ( < ) lst) && is_safe lst)
  |> List.length

let part2 input =
  input |> parse_input
  |> List.filter (fun line ->
         let len = List.length line in
         let rec try_removing_at i =
           if i >= len then
             false
           else
             let filtered = List.filteri (fun j _ -> i <> j) line in
             if (is_sorted ( > ) filtered || is_sorted ( < ) filtered) && is_safe filtered then
               true
             else
               try_removing_at (i + 1)
         in
         try_removing_at 0)
  |> List.length

let get_solution () = part2 (read_file "data/day-2.txt") |> print_int
