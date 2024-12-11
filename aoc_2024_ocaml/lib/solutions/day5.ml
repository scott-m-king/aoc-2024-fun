module IntSet = Set.Make (Int)

type before_set = IntSet.t

module Dict = Map.Make (Int)

type order_dict = before_set Dict.t

let add_order order key (left, right) =
  if key = left then
    IntSet.add right order
  else
    order

let add_to_dict map key elem =
  match Dict.find_opt key map with
  | Some order -> Dict.add key (add_order order key elem) map
  | None -> Dict.add key (add_order IntSet.empty key elem) map

let parse_ordering input =
  let map = Dict.empty in
  input |> String.split_on_char '\n'
  |> List.filter_map (fun str ->
         match String.split_on_char '|' str with
         | [ l; r ] -> Some (int_of_string l, int_of_string r)
         | _ -> None)
  |> List.fold_left (fun acc (l, r) -> add_to_dict (add_to_dict acc l (l, r)) r (l, r)) map

let parse_numbers input =
  input |> String.split_on_char '\n'
  |> List.map (fun str -> String.split_on_char ',' str |> List.map int_of_string)

let parse_input input =
  match Str.split (Str.regexp "\n\n") input with
  | [ ordering; numbers ] -> (parse_ordering ordering, parse_numbers numbers)
  | _ -> (Dict.empty, [])

let validate lst ordering =
  let rec aux result remaining =
    match remaining with
    | first :: second :: rest -> (
      match Dict.find first ordering |> IntSet.find_opt second with
      | Some _ -> aux result (second :: rest)
      | None -> false)
    | _ -> true
  in
  aux true lst

let part1 input =
  let ordering, numbers = parse_input input in
  numbers
  |> List.filter (fun lst -> validate lst ordering)
  |> List.fold_left (fun acc lst -> acc + List.nth lst (List.length lst / 2)) 0

let part2 _input = 0
let get_solution () = part1 (Utils.read_file "data/day-5.txt") |> print_int
