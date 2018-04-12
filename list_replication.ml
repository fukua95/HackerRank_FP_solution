let rec read_lines () = 
    try let line = read_line () in
        int_of_string (line) :: read_lines ()
    with 
        End_of_file -> []

let f n arr = 
    let rec g acc x arr = 
        match arr with
        | [] -> acc
        | hd :: tl -> if x = 0 then g acc n tl 
                      else g (hd :: acc) (x - 1) (hd :: tl)
    in
    let rec rev acc lst = 
        match lst with
        | [] -> acc
        | hd :: tl -> rev (hd :: acc) tl
    in
    rev [] (g [] n arr)

let () =
    let n :: arr = read_lines () in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;
