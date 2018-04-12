let rec read_lines () =
    try let now = read_int () in
        now :: read_lines ()
    with 
        End_of_file -> []

let length lst = 
    let rec f n lst = 
        match lst with
        | [] -> n
        | _ :: tl -> f (n + 1) tl
    in
    f 0 lst

let () =
    let lst = read_lines () in
    print_int (length lst)
