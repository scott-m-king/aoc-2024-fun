open Re

let mut_pattern = compile (Perl.re {|mul\((\d+),(\d+)\)|})
let a_pattern = compile (Perl.re {|do\(\)|})
let d_pattern = compile (Perl.re {|don't\(\)|})

let parse_input str =
  all mut_pattern str
  |> List.map (fun groups ->
         let l = Group.get groups 1 |> int_of_string in
         let r = Group.get groups 2 |> int_of_string in
         (l, r))

let get_by_regex pattern input action =
  all pattern input |> List.map (fun groups -> (Group.start groups 0, action))

let find_last_occurrences lst =
  let rec aux curr = function
    | [] -> []
    | (idx, action) :: rest when curr = "start" || action <> curr -> idx :: aux action rest
    | _ :: rest -> aux curr rest
  in
  aux "*" lst

let rec every_other = function
  | ([] | [ _ ]) as l -> l
  | x :: _ :: rest -> x :: every_other rest

let part1 input = parse_input input |> List.fold_left (fun acc (l, r) -> acc + (l * r)) 0

let part2 input =
  let positions =
    get_by_regex a_pattern input "a" @ [ (0, "a") ] @ get_by_regex d_pattern input "d"
  in

  positions
  |> List.sort (fun (a, _) (b, _) -> a - b)
  |> find_last_occurrences
  |> fun lst ->
  lst @ [ String.length input ]
  |> Utils.windows 2 |> every_other
  |> List.map (fun w ->
         let start = List.hd w in
         let len = List.nth w 1 - start in
         parse_input (String.sub input start len))
  |> List.flatten
  |> List.fold_left (fun acc (l, r) -> acc + (l * r)) 0

let get_solution () = part2 (Utils.read_file "data/day-3.txt") |> print_int
