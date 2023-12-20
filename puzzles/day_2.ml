(* PART 1 *)

let input_test_file = "inputs/2021/day_2_test.txt"
let input_file = "inputs/2021/day_2.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

type movement = {
  direction : string;
  value : int; 
}

let sum_movement move_lst = 
  let rec sum_movement' acc_forward acc_depth move_lst =
    match move_lst with 

    | h :: t -> sum_movement' (( 
        match h.direction with 
        | "forward" -> h.value
        | _ -> 0
      ) :: acc_forward) 
      (( 
        match h.direction with 
        | "forward" -> 0
        | "down" -> h.value * -1
        | "up" -> h.value
        | _ -> failwith("unexpected direction value")
      ) :: acc_depth) t

    | [] -> [List.fold_left (+) 0 acc_forward ; 
             -1 * List.fold_left (+) 0 acc_depth]

  in sum_movement' [] [] move_lst 

let solve_part1 file = 
  parse_content file 
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun x -> {direction = List.hd x ; 
                         value = int_of_string (List.hd (List.rev x))})
  |> sum_movement
  |> List.fold_left ( * ) 1

let result_test = solve_part1 input_test_file
let result = solve_part1 input_file

(* PART 2 *)

let sum_movement_2 move_lst = 
  let rec sum_movement' acc_forward acc_depth aim move_lst =
    match move_lst with 

    | h :: t -> sum_movement' (( 
      match h.direction with 
      | "forward" -> h.value
      | _ -> 0
    ) :: acc_forward)

    (( 
      match h.direction with 
      | "forward" -> aim * h.value
      | "down" -> 0
      | "up" -> 0
      | _ -> failwith("unexpected direction value")
    ) :: acc_depth) 

    (aim - ( 
      match h.direction with 
      | "forward" -> 0
      | "down" -> h.value * -1
      | "up" -> h.value
      | _ -> failwith("unexpected direction value")
    ))
    
    t

    | [] -> [List.fold_left (+) 0 acc_forward ; 
             List.fold_left (+) 0 acc_depth]

    (* | [] -> [List.rev acc_forward ; 
             List.rev acc_depth; [aim]] *)

  in sum_movement' [] [] 0 move_lst 

let solve_part2 file =  
  parse_content file
  |> List.map (fun x -> String.split_on_char ' ' x)
  |> List.map (fun x -> {direction = List.hd x ; 
                         value = int_of_string (List.hd (List.rev x))})
  |> sum_movement_2
  |> List.fold_left ( * ) 1

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file