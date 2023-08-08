open Printf

let file_name = "puzzle.txt"

let read_file file = 
  let chan = open_in file in 
  let rec f' lines =
    try
      f' lines @ [input_line chan]
    with End_of_file ->
      close_in chan;
      lines
  in f' []

let combined_weights lines = 
  let rec f' elves c lines =
    match lines with
    | [] -> elves
    | "" :: xs -> f' (elves @ [c]) 0 xs
    | x :: xs -> f' elves (c + int_of_string x) xs
  in f' [] 0 lines

let top n elves = 
  let sorted = List.sort (compare) elves in 
  let rec f' k lst =
    match lst with
    | [] -> lst
    | x :: xs -> if k = 1 then [x] else x :: f' (k-1) xs
  in f' n (List.rev sorted)

let solve file_name =
  let top3 = 
    read_file file_name 
    |> combined_weights 
    |> top 3 
  in
  printf "part 1:\t%d\n" @@ List.nth top3 0;
  printf "part 2:\t%d\n" @@ List.fold_left (+) 0 top3

let () = solve file_name

