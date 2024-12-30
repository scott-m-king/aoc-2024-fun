open Utils

let parse_input str =
  str |> String.trim |> String.split_on_char ' ' |> List.map Int64.of_string

let has_even_digits n = n |> Int64.to_string |> String.length |> fun len -> len mod 2 = 0

let split_stone n =
  n
  |> Int64.to_string
  |> fun x ->
  (x, String.length x / 2)
  |> fun (x, half_len) ->
  [ String.sub x 0 half_len; String.sub x half_len half_len ] |> List.map Int64.of_string

let get_next = function
  | stone when stone = Int64.zero -> [ Int64.of_int 1 ]
  | stone when has_even_digits stone -> split_stone stone
  | stone -> [ Int64.mul stone (Int64.of_int 2024) ]

let part1 input =
  let start = parse_input input in
  let iterations = 6 in
  let result =
    List.init iterations (fun x -> x)
    |> List.fold_left
         (fun stones_so_far _ ->
           stones_so_far |> List.fold_left (fun acc y -> acc @ get_next y) [])
         start
  in
  List.iter (fun x -> Printf.printf "%Ld " x) result ;
  0

let part2 _input = 0

let get_solution () = part1 (read_file "data/day-11-test.txt") |> print_int
