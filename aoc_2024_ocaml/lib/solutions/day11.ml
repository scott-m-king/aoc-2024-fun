open Utils

let parse_input str =
  str |> String.trim |> String.split_on_char ' ' |> List.map Int64.of_string

let has_even_digits n = n |> Int64.to_string |> fun x -> String.length x mod 2 = 0

let split_stone n =
  Int64.to_string n
  |> fun x ->
  (x, String.length x / 2)
  |> fun (x, half_len) ->
  [ String.sub x 0 half_len; String.sub x half_len half_len ] |> List.map Int64.of_string

let blink = function
  | stone when stone = 0L -> [ 1L ]
  | stone when has_even_digits stone -> split_stone stone
  | stone -> [ Int64.mul stone 2024L ]

(* Brute force *)
let part1 input =
  let iterations = 25 in
  parse_input input
  |> fun start ->
  List.init iterations (fun x -> x)
  |> List.fold_left
       (fun stones_so_far _ ->
         stones_so_far |> List.fold_left (fun acc y -> blink y @ acc) [])
       start
  |> List.length

let rec solve_memoized cache stone i =
  match i, Hashtbl.find_opt cache (i, stone) with
  | 0, _ -> 1
  | _, Some cached -> cached
  | _, None -> solve cache stone i

and solve cache stone i =
  blink stone
  |> List.fold_left (fun acc curr -> acc + solve_memoized cache curr (i - 1)) 0
  |> fun res ->
  Hashtbl.add cache (i, stone) res ;
  res

let part2 input =
  let memo : (int * int64, int) Hashtbl.t = Hashtbl.create 100 in
  parse_input input
  |> List.fold_left (fun acc curr -> acc + solve_memoized memo curr 75) 0

let get_solution () = part2 (read_file "data/day-11.txt") |> print_int
