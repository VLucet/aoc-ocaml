(* PART 1 *)

let input_test_file = "inputs/2021/day_7_test.txt"
let input_file = "inputs/2021/day_7.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char ',' contents
    |> List.map int_of_string

let make_distance_vec pos = 
  let range = (List.fold_right max pos 0) - 
              (List.fold_right min pos max_int) in
  List.init range (fun x -> x + 1)

let compute_differences vec value =
  List.map (fun x -> abs (x - value)) vec


let solve_part1 file = 
  let pos = parse_content file in 
  let range = make_distance_vec pos in
  let sums = List.map (compute_differences pos) range
             |> List.map (fun x -> List.fold_right ( + ) x 0) in
  let min_sum = List.fold_right min sums max_int in min_sum
  (* let range_idx =
    match List.find_index (fun x -> x = min_sum) sums with
    | Some a -> a
    | None -> failwith "no minimum matched"
  in List.nth range range_idx *)

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let compute_differences_2 vec value =
  let diff = List.map (fun x -> abs (x - value)) vec in
  List.map (fun x -> List.fold_right ( + ) (List.init x (fun y -> y + 1)) 0) diff

let solve_part2 file =  
  let pos = parse_content file in 
  let range = make_distance_vec pos in
  let sums = List.map (compute_differences_2 pos) range
             |> List.map (fun x -> List.fold_right ( + ) x 0) in
  let min_sum = List.fold_right min sums max_int in min_sum

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file