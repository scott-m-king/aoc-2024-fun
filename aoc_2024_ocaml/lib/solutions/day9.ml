open Utils

(* id; size; free_space *)
type position = int * int * int

type context =
  { start : position
  ; last : position
  ; result : int list
  }

type context2 =
  { i : int
  ; pos : position
  }

let get_positions lst =
  let rec aux curr acc =
    match curr with
    | [ x ] -> (x, 0) :: acc
    | x :: y :: rest -> aux rest ((x, y) :: acc)
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
  let { start = s_id, _, _; last = l_id, _, _; _ } = ctx in
  if s_id = l_id then ctx else get_next_result ctx lst

and get_next_result ctx lst =
  let { start = (_, _, s_free) as start; last = (l_id, l_size, _) as last; result } =
    ctx
  in
  match s_free, l_size with
  | free, size when free > 0 && size > 0 ->
    solve { start = sub_free start; last = sub_size last; result = l_id :: result } lst
  | free, 0 when free > 0 -> solve { start; last = find_second_last last lst; result } lst
  | 0, 0 -> { ctx with last = find_second_last last lst }
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

let init_map lst =
  List.fold_left
    (fun acc (id, _, _) ->
      Hashtbl.add acc id [] ;
      acc)
    (Hashtbl.create (List.length lst))
    lst

let add_to_map k v map =
  Hashtbl.find_opt map k
  |> Option.value ~default:[]
  |> (fun lst -> v :: lst)
  |> Hashtbl.replace map k ;
  map

let find pos lst =
  let filtered =
    lst
    |> List.filter (fun (id, _, free) ->
      let x_id, size, _ = pos in
      x_id > id && size <= free)
  in
  List.nth_opt filtered 0

let replace (x_id, x_size, x_free) (y_id, y_size, y_free) lst =
  lst
  |> List.fold_left
       (fun acc x ->
         match x with
         | id, _, _ when x_id = id -> (x_id, x_size, x_free - y_size) :: acc
         | id, _, _ when y_id = id -> (y_id, 0, y_free) :: acc
         | _ -> x :: acc)
       []
  |> List.rev

let get_res size id start_idx =
  List.init size (fun _ -> id)
  |> List.mapi (fun i x -> start_idx + i, x)
  |> List.fold_left
       (fun acc (idx, id) ->
         Int64.add acc (Int64.mul (Int64.of_int idx) (Int64.of_int id)))
       (Int64.of_int 0)

let rec block_checksum start_idx (free, positions) =
  free
  |> function
  | f when f > 0 ->
    (match positions with
     | [ (id, size, _) ] -> get_res size id start_idx
     | (id, size, _) :: xs ->
       Int64.add
         (get_res size id start_idx)
         (block_checksum (start_idx + size) (free - size, xs))
     | _ -> Int64.of_int 0)
  | _ -> Int64.of_int 0

let get_blocks lst =
  List.rev lst
  |> List.fold_left
       (fun (acc, new_list) curr ->
         let orig_id, _, _ = curr in
         match find curr new_list with
         | Some (id, size, free) ->
           add_to_map id curr acc, replace (id, size, free) curr new_list
         | None -> add_to_map orig_id curr acc, new_list)
       (init_map lst, lst)
  |> fun (res, _) -> res

let part2 input =
  let lst = parse_input input in
  let blocks = get_blocks lst in
  lst
  |> List.fold_left
       (fun acc curr ->
         let id, size, free = curr in
         (match Hashtbl.find blocks id with
          | x :: xs when x = curr -> size + free, x :: List.rev xs
          | xs -> size + free, (0, size, free) :: List.rev xs)
         :: acc)
       []
  |> List.rev
  |> List.fold_left
       (fun (start_idx, acc) (total_space, positions) ->
         let next_idx = start_idx + total_space in
         let checksum = block_checksum start_idx (total_space, positions) in
         next_idx, Int64.add acc checksum)
       (0, Int64.of_int 0)
  |> fun (_, res) -> res

let get_solution () = part2 (read_file "data/day-9.txt") |> Printf.printf "\n%Ld\n"
