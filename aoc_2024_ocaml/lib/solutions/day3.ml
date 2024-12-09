open Re

let mut_pattern = compile (Perl.re {|mul\((\d+),(\d+)\)|})

let parse_input str =
  all mut_pattern str
  |> List.map (fun groups ->
         (int_of_string (Group.get groups 1), int_of_string (Group.get groups 2)))

let part1 input =
  parse_input input |> List.fold_left (fun acc (l, r) -> acc + (l * r)) 0

let part2 = 0
let get_solution () = part1 (Utils.read_file "data/day-3.txt") |> print_int
