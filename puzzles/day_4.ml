(* PART 1 *)
let input_test_file = "inputs/2021/day_4_test.txt"
let input_file = "inputs/2021/day_4.txt"

let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let parse_content file =
  let contents = read_file file in
    String.split_on_char '\n' contents

let make_id_list lst by start =
  let max_length = List.length lst in 
    let rec make_id_list' acc by lst =
      match (List.hd acc + by) < max_length with
      | true -> make_id_list' ((List.hd acc + by) :: acc) by lst 
      | false -> List.rev acc
    in  make_id_list' [start] by lst

let parse_header lst =
  String.split_on_char ',' lst
  |> List.map int_of_string 

let extract lst id_1 id_2 = 
  let lst_length = id_2 - id_1 in 
    let rec extract' acc id lst =
      match lst_length > List.length acc with 
      | true -> extract' ((List.nth lst id) :: acc) (id + 1) lst
      | false -> List.rev acc  
    in extract' [List.nth lst id_1] (id_1 + 1) lst   

let parse_cards lst card_length by start = 
  let starts = make_id_list lst by start in 
    let ends = List.map (fun x -> x + card_length) starts in
      let cards = List.map2 (extract lst) starts ends in 
      cards 
      |> List.map (fun x -> List.map (fun x -> String.split_on_char ' ' x) x)
      |> List.map (fun x -> List.map (fun x -> List.filter (fun x -> not (x = "")) x) x)
      |> List.map (fun x -> List.map (fun x -> List.map (fun x -> int_of_string x) x) x)

let difference l1 l2 = List.filter (fun e -> not (List.mem e l2)) l1 

(* ******************************************************************************** *)

let get_lengths lst = 
  List.map (fun x -> List.map (fun x -> List.length x) x) lst

let reshape_list lst =
  let lst_length = List.length (List.hd lst) in
    let rec reshape_list' acc id lst =  
      match id with 
      | -1 -> acc
      | _ -> reshape_list' 
        ((List.map (fun x -> List.nth x id) lst) :: acc) (id - 1) lst
    in reshape_list' [] (lst_length - 1) lst

let find_zero_lengths lst = 
  let lst_lengths = get_lengths lst in 
    List.map (fun x -> List.find_index (fun x -> x = 0) x) lst_lengths
    (* |> List.find_index (fun x -> Option.is_some x) *)

let compute_score num id cards = 
  List.nth cards id 
  |> List.flatten 
  |> List.fold_left ( + ) 0
  |> ( * ) num

let check_presence header cards =
  let rec check_presence' acc header cards = 
    match header with 
    | h :: t -> (
        let cards_diff_row = 
          (List.map (fun x -> List.map (fun x -> difference x (h :: acc)) x)) cards in
        let cards_diff_col = 
          (List.map (fun x -> List.map (fun x -> difference x (h :: acc)) x) (List.map reshape_list cards)) in
        let zero_id_found_row = find_zero_lengths cards_diff_row in
        let zero_id_found_col = find_zero_lengths cards_diff_col in
          (* ([zero_id_found_row ; zero_id_found_col]) *)
          match zero_id_found_row, zero_id_found_col with 
            | None, None -> check_presence' (h :: acc) t cards
            | Some i, Some j -> compute_score h (min i j) cards_diff_row
            | None, Some i -> compute_score h i cards_diff_row
            | Some j, None -> compute_score h j cards_diff_row
      )
    |  [] -> failwith "no winner found"
  in check_presence' [] (header) cards

let solve_part1 file = 
  let content = parse_content file in 
    let header = parse_header (List.hd content) in
      let cards = parse_cards content 5 6 2 in
       check_presence header cards

let result_test = solve_part1 input_test_file

let result = solve_part1 input_file

(* PART 2 *)

let check_presence2 header cards =
  let rec check_presence' acc_h acc_wins header cards = 
    match header with 
    | h :: t -> (
        let cards_diff_row = 
          (List.map (fun x -> List.map (fun x -> difference x (h :: acc_h)) x)) cards in
        let cards_diff_col = 
          (List.map (fun x -> List.map (fun x -> difference x (h :: acc_h)) x) (List.map reshape_list cards)) in
        let zero_id_found_row = find_zero_lengths cards_diff_row in
        let zero_id_found_col = find_zero_lengths cards_diff_col in
          match zero_id_found_row, zero_id_found_col with 
            | None, None -> check_presence' (h :: acc_h) acc_wins t cards
            | Some i, Some j -> check_presence' (h :: acc_h)
                                                ([h ; j ; (compute_score h j cards_diff_col)] :: 
                                                  ([h ; i ; (compute_score h i cards_diff_row)] :: acc_wins))
                                                t cards 
            | None, Some i -> check_presence' (h :: acc_h)
                                              ([h ; i ; (compute_score h i cards_diff_row)] :: acc_wins) 
                                              t cards 
            | Some j, None -> check_presence' (h :: acc_h) 
                                              ([h ; j ; (compute_score h j cards_diff_col)] :: acc_wins) 
                                              t cards 
      )
    |  [] -> List.rev acc_wins
  in check_presence' [] [] (header) cards

let solve_part2 file = 
  let content = parse_content file in 
    let header = parse_header (List.hd content) in
      let cards = parse_cards content 5 6 2 in
       let wins = check_presence2 header cards in 
        let series = List.map (fun x -> List.nth x 1) wins in 
        [wins ; [series]]

let result_test_2 = solve_part2 input_test_file
let result_2 = solve_part2 input_file