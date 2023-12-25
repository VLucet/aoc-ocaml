(* PART 1 *)

let input_test_file = "inputs/2021/day_5_test.txt"
let input_file = "inputs/2021/day_5.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

let solve_part1 file = 
  parse_content file

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let solve_part2 file =  
  parse_content file

let result_test_2 = solve_part2 input_test_file
