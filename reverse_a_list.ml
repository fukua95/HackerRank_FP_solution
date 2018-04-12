let rec read_lines () = 
    try let now = read_int () in
        now :: read_lines ()
    with
        End_of_file -> []

let rev lst = 
    let rec f acc lst = 
        match lst with
        | [] -> acc
        | hd :: tl -> f (hd :: acc) tl
    in
    f [] lst

let () = 
    let lst = read_lines () in
    let res = rev lst in
    List.iter (fun x -> print_int x; print_newline ()) res
