(* PART 1 *)

let input_test_file = "inputs/2021/day_5_test.txt"
let input_file = "inputs/2021/day_5.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

type line_type = | Vert | Hori | Diag
type point = { c     : int   ; r    : int ; }
type line  = { first : point ; last : point ; cat : line_type}

let to_point x =
  let pair = 
    String.split_on_char ',' x 
    |> List.map (fun x -> int_of_string x)
  in match pair with
  | h :: t -> { c = h ; r = List.hd t }
  | []     -> failwith "improper point data"
  
let match_cat first last =
  match first.c = last.c, first.r = last.r with
  | true  , false -> Vert
  | false , true  -> Hori
  | false , false -> Diag
  | true  , true  -> failwith "line is made of a single point!" 

let parse_line line =
  let point_pair = 
    line
    |> String.split_on_char ' '
    |> List.filter (fun x -> not (x = "->"))
    |> List.map to_point
  in match point_pair with 
    | h :: t -> { first = h ; last = List.hd t ; cat = match_cat h (List.hd t) }
    | []     -> failwith "improper line data"

let to_point_list line =
  match line.cat with 
  | Hori -> let diff_cols = (Int.abs (line.last.c - line.first.c)) + 1 in
            let col_ids = List.init diff_cols (fun x -> Int.abs (x + 
              if line.first.c < line.last.c then line.first.c else -line.first.c)) in
            let row_ids = List.init diff_cols (fun x -> line.first.r) in
            List.map2 (fun c r -> {c = c ; r = r}) col_ids row_ids
  | Vert -> let diff_rows = (Int.abs (line.last.r - line.first.r)) + 1 in 
            let col_ids = List.init diff_rows (fun x -> line.first.c) in
            let row_ids = List.init diff_rows (fun x -> Int.abs (x + 
              if line.first.r < line.last.r then line.first.r else -line.first.r)) in
            List.map2 (fun c r -> {c = c ; r = r}) col_ids row_ids
  | Diag -> let diff_rows = (Int.abs (line.last.r - line.first.r)) + 1 in
            let diff_cols = (Int.abs (line.last.c - line.first.c)) + 1 in
            let col_ids = List.init diff_cols (fun x -> Int.abs (x + 
              if line.first.c < line.last.c then line.first.c else -line.first.c)) in
            let row_ids = List.init diff_rows (fun x -> Int.abs (x + 
              if line.first.r < line.last.r then line.first.r else -line.first.r)) in
            List.map2 (fun c r -> {c = c ; r = r}) col_ids row_ids

let find_duplicates lst = 
  let rec find_duplicates' acc lst =
    match lst with 
    | h :: m :: rest -> if h = m then find_duplicates' (h :: acc) (m :: rest) 
                                 else find_duplicates' acc (m :: rest)
    | h :: [] -> if h = List.hd acc then find_duplicates' (h :: acc) [] 
                                    else find_duplicates' acc []
    | [] -> List.rev acc |> List.sort_uniq compare
  in find_duplicates' [] lst

let solve_part1 file = 
  parse_content file
  |> List.map parse_line
  |> List.filter (fun x -> (x.cat = Vert) || (x.cat = Hori))
  |> List.map to_point_list
  |> List.flatten
  |> List.fast_sort compare
  |> find_duplicates

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file 
let result_length = result |> List.length

(* PART 2 *)

let solve_part2 file =  
  parse_content file
  |> List.map parse_line
  (* |> List.filter (fun x -> (x.cat = Vert) || (x.cat = Hori)) *)
  |> List.map to_point_list
  |> List.flatten
  |> List.fast_sort compare
  |> find_duplicates

let result_test_2 = solve_part2 input_test_file

let result_2 = solve_part2 input_file

let result_length_2 = result_2 |> List.length