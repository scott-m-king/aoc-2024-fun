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

let rec build_acc (x, y) acc =
  match x, y with
  | (x_id, x_size, x_free), (y_id, y_size, y_free) when x_free > 0 && y_size > 0 ->
    build_acc ((x_id, x_size, x_free - 1), (y_id, y_size - 1, y_free)) (y_id :: acc)
  | _ -> x, y, acc

let sub_free (id, size, free) = id, size, free - 1

let sub_size (id, size, free) = id, size - 1, free

(* requires reverse *)
let rec find_second_last ((id, size, free) : position) (lst : position list) : position =
  match lst with
  | [] | [ _ ] -> id, size, free
  | (x_id, _, _) :: y :: _ when x_id = id -> y
  | _ :: xs -> find_second_last (id, size, free) xs

let rec solve ctx lst : context =
  let { start; last; result } = ctx in
  match start, last with
  | (start_id, _, _), (last_id, _, _) when start_id = last_id -> ctx
  | (_, _, x_free), (y_id, y_size, _) when x_free > 0 && y_size > 0 ->
    solve { start = sub_free start; last = sub_size last; result = y_id :: result } lst
  | (_, _, x_free), (_, y_size, _) when x_free > 0 && y_size = 0 ->
    solve { start; last = find_second_last last (List.rev lst); result } lst
  | (_, _, x_free), (_, y_size, _) when x_free = 0 && y_size = 0 ->
    { start; last = find_second_last last (List.rev lst); result }
  | _ -> ctx

let act { last; result; _ } curr lst =
  let init = build_init curr result in
  solve { start = curr; last; result = init } lst

let lambda ctx curr lst =
  let { start; last; result } = ctx in
  match start, last with
  | _, (id, _, _) when id = -1 -> ctx
  | (x_id, _, _), (y_id, _, _) when x_id = y_id -> ctx
  | (x_id, _, _), (y_id, _, _) when x_id = y_id - 1 ->
    { start; last = -1, -1, -1; result = build_init last result }
  | _ -> act ctx curr lst

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

let get_solution () = part1 (read_file "data/day-9-test.txt") |> Printf.printf "\n%Ld\n"
