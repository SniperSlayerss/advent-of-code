let find_sum_from_string s =
    let (a,b) = Aoc_2024.Utils.lines_to_int_array s in
    let rec get_dists a b acc = 
        match a, b with
            | fa :: a, fb :: b -> get_dists a b (Int.abs (fa - fb) :: acc)
            | _ -> acc
    in  
    let dists = get_dists a b [] in
    List.fold_right (+) dists 0

let occurences n l =
    List.length (List.filter (fun x -> x = n) l)

let find_similarity_from_string s =
    let (a,b) = Aoc_2024.Utils.lines_to_int_array s in
    let rec get_similarity a acc = 
        match a with
            | fa :: rest -> get_similarity rest ((occurences fa b) * fa :: acc)
            | [] -> acc
    in  
    let similarity = get_similarity a [] in
    List.fold_right (+) similarity 0
     

let () = 
    let filename = "data/day01.txt" in 
    let contents = Aoc_2024.Utils.read_file_contents filename in
    let sum = find_sum_from_string contents in
    let similarity = find_similarity_from_string contents in
    Printf.printf "Sum: %d\nSimilarity: %d\n" sum similarity 
