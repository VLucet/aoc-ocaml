(* PART 1 *)

let input_test_file = "inputs/2021/day_1_test.txt"
let input_file = "inputs/2021/day_1.txt"

(* read the entire file *)
let read_file file =
  In_channel.with_open_bin file In_channel.input_all

(* read lines *)
let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

let compute_differences int_list =
  let rec differences acc int_list = 
  match int_list with
    | x :: y :: rest -> differences ((y - x) :: acc) (y :: rest)
    | _ -> List.rev acc
  in
  differences [] int_list

let solve_part1 file = 
  parse_content file 
  |> List.map int_of_string
  |> compute_differences
  |> List.filter (fun x -> x > 0)
  |> List.length

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let take n lst = 
  let rec take' n acc lst =
    match n, lst with 
	    | 0, _ -> List.rev acc
      | _, [] -> failwith("m < List.length lst")
      | _, h :: t -> take' (n-1) (h :: acc) t
  in take' n [] lst

let sum_by n lst =
  let rec sum_by' n acc lst =
    match (List.length lst >= n), n, lst with
      | true,  _, h :: t -> 
        sum_by' n ((List.fold_left (+) 0 (take n lst)) :: acc) t
      | false, _, _      -> List.rev acc
      | _,     _, _      -> failwith("error")
  in sum_by' n [] lst

let solve_part2 file =  
  parse_content file 
  |> List.map int_of_string
  |> sum_by 3
  |> compute_differences
  |> List.filter (fun x -> x > 0)
  |> List.length

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file
