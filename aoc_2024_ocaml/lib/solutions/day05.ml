open Utils
module IntSet = Set.Make (Int)

type before_set = IntSet.t

module Dict = Map.Make (Int)

type order_dict = before_set Dict.t

let add_order order key (left, right) =
  if key = left then IntSet.add right order else order

let add_to_dict map key elem =
  match Dict.find_opt key map with
  | Some order -> Dict.add key (add_order order key elem) map
  | None -> Dict.add key (add_order IntSet.empty key elem) map

let parse_ordering input =
  input
  |> String.split_on_char '\n'
  |> List.filter_map (fun str ->
    match String.split_on_char '|' str with
    | [ l; r ] -> Some (int_of_string l, int_of_string r)
    | _ -> None)
  |> List.fold_left
       (fun acc (l, r) -> add_to_dict (add_to_dict acc l (l, r)) r (l, r))
       Dict.empty

let parse_updates input =
  input
  |> String.split_on_char '\n'
  |> List.map (fun str -> String.split_on_char ',' str |> List.map int_of_string)

let parse_input input =
  match Str.split (Str.regexp "\n\n") input with
  | [ ordering; numbers ] -> parse_ordering ordering, parse_updates numbers
  | _ -> Dict.empty, []

let is_valid_ordering lst ordering =
  let rec aux result remaining =
    match remaining with
    | first :: second :: rest ->
      (match Dict.find first ordering |> IntSet.find_opt second with
       | Some _ -> aux result (second :: rest)
       | None -> false)
    | _ -> true
  in
  aux true lst

let sort_by_ordering lst ordering =
  lst
  |> List.sort (fun a b ->
    match Dict.find b ordering |> IntSet.find_opt a with
    | Some _ -> 1
    | None -> -1)

let part1 input =
  let ordering, all_updates = parse_input input in
  all_updates
  |> List.filter (fun updates -> is_valid_ordering updates ordering)
  |> List.fold_left
       (fun acc updates -> acc + List.nth updates (List.length updates / 2))
       0

let part2 input =
  let ordering, all_updates = parse_input input in
  all_updates
  |> List.filter (fun updates -> not (is_valid_ordering updates ordering))
  |> List.map (fun updates -> sort_by_ordering updates ordering)
  |> List.fold_left
       (fun acc updates -> acc + List.nth updates (List.length updates / 2))
       0

let get_solution () = part2 (read_file "data/day-5.txt") |> print_int
