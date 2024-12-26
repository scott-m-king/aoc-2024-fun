open Utils

(* id; size; free_space *)
type position = int * int * int

type context =
  { start : position
  ; last : position
  ; result : int list
  }

let print_position (x, y, z) = Printf.printf "(%d,%d,%d)" x y z

let print_context ctx =
  Printf.printf "{ start = " ;
  print_position ctx.start ;
  Printf.printf "; last = " ;
  print_position ctx.last ;
  Printf.printf
    "; result = [%s] }"
    (String.concat ";" (List.map string_of_int ctx.result)) ;
  print_char '\n'

let get_positions lst =
  let rec aux curr acc =
    match curr with
    | x :: y :: rest -> aux rest ((x, y) :: acc)
    | [ x ] -> (x, 0) :: acc
    | _ -> acc
  in
  aux lst []

let parse_input str =
  String.fold_right
    (fun c acc -> (int_of_char c - int_of_char '0') :: acc)
    (String.trim str)
    []
  |> get_positions
  |> List.rev
  |> List.mapi (fun i (x, y) -> i, x, y)

let rec last = function
  | [] -> 0, 0, 0
  | [ x ] -> x
  | _ :: rest -> last rest

let build_init (id, size, _) acc = List.init size (fun _ -> id) |> fun x -> x @ acc

let sub_free (id, size, free) = id, size, free - 1

let sub_size (id, size, free) = id, size - 1, free

let find_second_last ((id, _, _) as pos) lst =
  let rec aux _ = function
    | [] | [ _ ] -> pos
    | (x_id, _, _) :: y :: _ when x_id = id -> y
    | _ :: xs -> aux pos xs
  in
  aux pos (List.rev lst)

let rec solve ctx lst =
  let { start = (s_id, _, s_free) as start; last = (l_id, l_size, _) as last; result } =
    ctx
  in
  match s_id = l_id, s_free, l_size with
  | true, _, _ -> ctx
  | _, free, size when free > 0 && size > 0 ->
    solve { start = sub_free start; last = sub_size last; result = l_id :: result } lst
  | _, free, 0 when free > 0 ->
    solve { start; last = find_second_last last lst; result } lst
  | _, 0, 0 -> { ctx with last = find_second_last last lst }
  | _ -> ctx

let lambda ctx curr lst =
  let { start = (s_id, _, _) as start; last = (l_id, _, _) as last; result } = ctx in
  match l_id = -1, s_id, l_id with
  | true, _, _ -> ctx
  | _, x_id, y_id when x_id = y_id -> ctx
  | _, x_id, y_id when x_id = y_id - 1 ->
    { start; last = -1, -1, -1; result = build_init last result }
  | _ -> solve { start = curr; last; result = build_init curr result } lst

let part1 input =
  let lst = parse_input input in
  let init = { start = List.hd lst; last = last lst; result = [] } in
  lst
  |> List.fold_left (fun ctx curr -> lambda ctx curr lst) init
  |> (fun x -> List.rev x.result)
  |> List.map (fun x -> Int64.of_int x)
  |> List.fold_left
       (fun (i, acc) curr -> i + 1, Int64.add acc (Int64.mul (Int64.of_int i) curr))
       (0, Int64.of_int 0)
  |> fun (_, x) -> x

let part2 _input = 0

let get_solution () = part1 (read_file "data/day-9.txt") |> Printf.printf "\n%Ld\n"
