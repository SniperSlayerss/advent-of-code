let parse_line_int s =
    let re = Str.regexp "[0-9]+" in
    let rec collect_matches acc pos =
        try
            let pos = Str.search_forward re s pos in
            let num = Str.matched_string s in
            collect_matches (int_of_string num :: acc) (pos + String.length num)
        with Not_found ->
            List.rev acc
        in
        collect_matches [] 0
    
let convert_to_array s =
    let split_lines = String.split_on_char '\n' s in
    let split_ints = List.map parse_line_int split_lines in
    let rec combine lines acca accb = 
        match lines with
            | [a ; b] :: rest -> combine rest (a :: acca) (b :: accb)
            | _ -> (List.sort compare acca, List.sort compare accb)
    in
    combine split_ints [] []

(* let find_sum_from_string s =
    let (a,b) = convert_to_array s in
    let rec get_dists a b acc = 
        match a, b with
            | fa :: a, fb :: b -> get_dists a b (Int.abs (fa - fb) :: acc)
            | _ -> acc
    in  
    let dists = get_dists a b [] in
    List.fold_right (+) dists 0 *)


let occurences n l =
    List.length (List.filter (fun x -> x = n) l)

let find_similarity_from_string s =
    let (a,b) = convert_to_array s in
    let rec get_similarity a acc = 
        match a with
            | fa :: rest -> get_similarity rest ((occurences fa b) * fa :: acc)
            | [] -> acc
    in  
    let similarity = get_similarity a [] in
    List.fold_right (+) similarity 0
     

let () = 
    let filename = "data/day01.txt" in 
    let contents = Aoc_2024.Utils.read_file_as_string filename in
    let sum = find_similarity_from_string contents in
    Printf.printf "%d\n" sum
