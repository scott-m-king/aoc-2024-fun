let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_pair_lists (lst1, lst2) =
  let list_to_string lst =
    "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]"
  in
  Printf.printf "(%s, %s)\n" (list_to_string lst1) (list_to_string lst2)

let print_list lst =
  "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]"
  |> print_endline

let print_pairs lst =
  Format.printf "[%a]@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%d, %d)" x y))
    lst
