let rec read_lines () =
    try let now = read_int () in
        now :: read_lines ()
    with
        End_of_file -> []

let filter x lst = 
    let rec f acc x lst = 
        match lst with
        | [] -> acc
        | hd :: tl -> if hd < x then f (hd :: acc) x tl
                      else f acc x tl
    in 
    let rec rev acc lst = 
        match lst with
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (f [] x lst)

let () =
    let x = read_int () in
    let lst = read_lines () in
    let res = filter x lst in
    List.iter (fun x -> print_int x; print_newline ()) res

