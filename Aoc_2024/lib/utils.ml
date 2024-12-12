let read_file_contents filename = 
    let ic = open_in filename in 
    try
        let contents = really_input_string ic (in_channel_length ic) in
        close_in ic;
        contents
    with e ->
        close_in_noerr ic;
        raise e

let line_to_int_array s =
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


let lines_to_int_array_tuple s =
    let split_lines = String.split_on_char '\n' s in
    let split_ints = List.map line_to_int_array split_lines in
    let rec combine lines acca accb = 
        match lines with
            | [a ; b] :: rest -> combine rest (a :: acca) (b :: accb)
            | _ -> (List.sort compare acca, List.sort compare accb)
    in
    combine split_ints [] [] 

let lines_to_ints s =
    let split_lines = String.split_on_char '\n' s in
    List.map line_to_int_array split_lines
        