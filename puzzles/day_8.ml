(* PART 1 *)

let input_test_file = "inputs/2021/day_8_test.txt"
let input_file = "inputs/2021/day_8.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

let length_to_digit lgt =
  match lgt with 
  | 2 -> 1
  | 3 -> 7
  | 4 -> 4
  | 7 -> 8
  | _ -> -1
 
let solve_part1 file = 
  parse_content file
  |> List.map (String.split_on_char '|')
  |> List.map (fun x -> List.nth x 1)
  |> List.map (String.split_on_char ' ')
  |> List.map (fun x -> List.filter (fun y -> y <> "") x)
  |> List.map (fun x -> List.map String.length x)
  |> List.map (fun x -> List.map length_to_digit x)
  |> List.map (fun x -> List.filter (fun y -> y >= 0) x)
  |> List.flatten
  |> List.length

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let solve_part2 file =  
  parse_content file

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file