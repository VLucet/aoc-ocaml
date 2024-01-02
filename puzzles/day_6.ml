(* PART 1 *)

(* module SI = Set.Make(Int) *)

let input_test_file = "inputs/2021/day_6_test.txt"
let input_file = "inputs/2021/day_6.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents
    |> List.hd

let parse_initial_pop line =
  String.split_on_char ',' line 
  |> List.map (fun x -> int_of_string x)

let count_equal values lst =
  let rec count_equal' acc values lst =
    match values with 
    | h :: t -> count_equal' ((
      List.filter (fun x -> x = h) lst
      |> List.length
    ) :: acc) t lst
    | [] -> List.rev acc
  in count_equal' [] values lst

let rec run_simulation steps pop =
  match steps > 0, pop with 
  | true , hd :: tl -> run_simulation (steps - 1) (
      match tl with 
      | a :: b :: c :: d :: e :: f :: g :: h -> 
          a :: b :: c :: d :: e :: f :: (g + hd) :: List.hd h :: [hd]
      | _ -> failwith "unreachable"
    ) 
  | false, _ -> pop
  | _ , _ -> failwith "unreachable"

let solve_part1 file = 
  parse_content file
  |> parse_initial_pop 
  |> count_equal (List.init 9 (fun x -> x))
  |> run_simulation 80
  |> List.fold_left ( + ) 0

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let solve_part2 file =  
  parse_content file
  |> parse_initial_pop 
  |> count_equal (List.init 9 (fun x -> x))
  |> run_simulation 256
  |> List.fold_left ( + ) 0

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file