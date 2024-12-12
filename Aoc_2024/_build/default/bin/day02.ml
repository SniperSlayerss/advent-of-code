let differences arr = 
  let rec aux a acc = 
    match a with
      | x :: y :: rest -> aux rest (x - y :: acc)
      | _ -> []
  in
  aux arr [] 

let cut = function
  | a::arr -> (a, arr)
  | _ -> (0,[])

let check_safety arr =
  let (first, rest) = cut arr in 
  let rec aux a prev =
    match a with
      | x :: rest when (prev < 0 && prev * x < 0) || (prev > 0 && prev * x > 0)  -> aux rest x
      | [] -> true
      | _ -> false
  in
  aux rest first

let part_1 s =
  let arr = Aoc_2024.Utils.lines_to_ints s in
  let diff_arr = List.map differences arr in
  let safety_arr = List.map check_safety diff_arr in
  let number_of_safe = List.length (List.filter (fun x -> x = true) safety_arr) in
  Printf.printf "%d\n" number_of_safe 

let () =
    let filename = "data/day02.txt" in 
    let contents = Aoc_2024.Utils.read_file_contents filename in
    part_1 contents
