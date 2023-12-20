(* PART 1 *)

let input_test_file = "inputs/2021/day_3_test.txt"
let input_file = "inputs/2021/day_3.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

let string_to_int_list s =
  s |> String.to_seq 
    |> List.of_seq 
    |> List.map Char.escaped 
    |> List.map int_of_string

let reshape_list lst =
  let lst_length = List.length (List.hd lst) in
    let rec reshape_list' acc id lst =  
      match id with 
      | -1 -> acc
      | _ -> reshape_list' 
        ((List.map (fun x -> List.nth x id) lst) :: acc) (id - 1) lst
    in reshape_list' [] (lst_length - 1) lst

let int_of_bool b = if b then 1 else 0

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let lst_to_num lst =
  let rec lst_to_num' i lst =
  match lst with
  | [] -> 0
  | h :: t -> (h * (pow 2 i)) + lst_to_num' (i - 1) t
  in lst_to_num' ((List.length lst)-1) lst

let get_num_1 file = 
  parse_content file
  |> List.map string_to_int_list
  |> reshape_list
  |> List.map (fun x -> List.partition (fun x -> x == 0) x)
  |> List.map (fun x -> List.length (fst x) < List.length (snd x))
  (* |> List.map (fun x -> [List.length (fst x) ; List.length (snd x)]) *)

let get_num_2 lst = 
  List.map (fun x -> not x) lst

let solve_part1 file = 
  let num1 = get_num_1 file in
    let num2_trans = get_num_2 num1
      |> List.map int_of_bool
      |> lst_to_num in 
    let num1_trans = num1 
      |> List.map int_of_bool
      |> lst_to_num  in
      num1_trans * num2_trans

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let which_greater_or_lesser which lst id =
  match which with
  | "greater" -> let x = 
    List.map (fun x -> List.nth x id) lst |>
      List.partition (fun x -> x == 0)
    in (List.length (fst x) <= List.length (snd x)) |> int_of_bool
  | "lesser" -> let x = 
    List.map (fun x -> List.nth x id) lst |>
      List.partition (fun x -> x == 0)
    in (List.length (fst x) > List.length (snd x)) |> int_of_bool
  | _ -> failwith("only 'greater' or 'lesser' allowed")

let filter_down g_or_l lst =
  let rec filter_down' g_or_l id lst =  
    match (List.length lst), id with 
    | 1, _ -> List.hd lst
    | _, _ -> filter_down' g_or_l (id + 1) ( 
      (* filter lst here *)
      List.filter (fun x -> (
        List.nth x id) == (which_greater_or_lesser g_or_l lst id)
      ) lst
    ) 
  in filter_down' g_or_l 0 lst

(* let solve_part2 file =
  parse_content file
  |> List.map string_to_int_list
  |> filter_down 
  |> lst_to_num *)

let get_num_1_p2 file = 
  parse_content file
  |> List.map string_to_int_list
  |> filter_down "greater"
  |> lst_to_num

let rec get_num_2_p2 file = 
  parse_content file
  |> List.map string_to_int_list
  |> filter_down "lesser"
  |> lst_to_num

let solve_part2 file = 
  let num1_trans = get_num_1_p2 file in
    let num2_trans = get_num_2_p2 file in
      num1_trans * num2_trans

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file