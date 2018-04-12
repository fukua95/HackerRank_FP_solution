let rec read_lines () = 
    try let now = read_int () in
        now :: read_lines ()
    with
        End_of_file -> []

let rec sum lst = 
    match lst with
    | [] -> 0
    | hd :: tl -> if hd mod 2 = 0 then sum tl
                  else hd + sum tl

let () =
    let lst = read_lines () in
    print_int (sum lst)
