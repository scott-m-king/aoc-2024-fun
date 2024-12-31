let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  s

let print_pair_lists (lst1, lst2) =
  let list_to_string lst =
    "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]"
  in
  Printf.printf "(%s, %s)\n" (list_to_string lst1) (list_to_string lst2)

let print_list lst =
  "[" ^ (List.map string_of_int lst |> String.concat "; ") ^ "]" |> print_endline

let print_str_list lst = List.iter (Printf.printf "elem: %s\n") lst

let print_tuple_list lst =
  Format.printf "@[<h>[" ;
  lst |> List.iter (fun (x, y) -> Format.printf "(%d,%d);@ " x y) ;
  Format.printf "]@]@\n"

let print_triple_list lst =
  Format.printf "@[<h>[" ;
  lst |> List.iter (fun (x, y, z) -> Format.printf "(%d,%d,%d);@ " x y z) ;
  Format.printf "]@]@\n"

let print_int_list_list lst =
  Printf.printf "[\n" ;
  List.iter
    (fun inner_list ->
      Printf.printf "  [" ;
      List.iter (fun x -> Printf.printf "%d; " x) inner_list ;
      Printf.printf "]\n")
    lst ;
  Printf.printf "]\n"

let print_int_pairs lst =
  Format.printf
    "[%a]@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%d, %d)" x y))
    lst

let print_str_pairs lst =
  Format.printf
    "[%a]@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (x, y) -> Format.fprintf fmt "(%s, %s)" x y))
    lst

let print_char_grid lst =
  Array.iter
    (fun inner_list ->
      Array.iter (fun c -> Printf.printf "%c" c) inner_list ;
      Printf.printf "\n")
    lst ;
  Printf.printf "\n"

let print_int_grid lst =
  Printf.printf "\n" ;
  Array.iter
    (fun inner_list ->
      Array.iter (fun c -> Printf.printf "%d " c) inner_list ;
      Printf.printf "\n")
    lst ;
  Printf.printf "\n"

module IntSet = Set.Make (Int)

type before = IntSet.t

module Dict = Map.Make (Int)

type order_dict = before Dict.t

type cell =
  { l : char
  ; x : int
  ; y : int
  }

type int_cell =
  { value : int
  ; row : int
  ; col : int
  }

let print_cell_list_vertical cells =
  print_endline "Corners:" ;
  List.iteri
    (fun i cell ->
      Printf.printf "  %d: { letter: '%c'; x: %d; y: %d }\n" i cell.l cell.x cell.y)
    cells

let make_indices grid =
  let h = Array.length grid in
  let w = Array.length grid.(0) in
  List.init h (fun y -> List.init w (fun x -> x, y)) |> List.flatten

let parse_grid input =
  input
  |> String.split_on_char '\n'
  |> List.map (fun str -> String.to_seq str |> Array.of_seq)
  |> Array.of_list

let parse_grid_with_indices input =
  let grid = parse_grid input in
  grid, make_indices grid

let get_cell grid (x, y) =
  try Some { l = grid.(x).(y); x; y } with
  | Invalid_argument _ -> None

let get_int_cell grid (row, col) =
  try Some { value = grid.(row).(col); row; col } with
  | Invalid_argument _ -> None

let print_cell (cell : cell) =
  Printf.printf "{letter: %c, x: %d, y: %d}\n" cell.l cell.x cell.y

let print_int_cell (cell : int_cell) =
  Printf.printf "{letter: %d, row: %d, col: %d}\n" cell.value cell.row cell.col

let rec take n = function
  | first :: rest when n > 0 -> first :: take (n - 1) rest
  | _ -> []

let windows n list =
  let rec aux acc = function
    | [] -> List.rev acc
    | lst when List.length lst < n -> List.rev acc
    | first :: rest -> aux (take n (first :: rest) :: acc) rest
  in
  aux [] list

let print_i64 n = Printf.printf " %Ld\n" n
