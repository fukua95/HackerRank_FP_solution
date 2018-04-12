let rec read_lines () = 
    try let now = read_int () in
        now :: read_lines ()
    with 
        End_of_file -> []

let remove_odd_position lst = 
    let rec f p lst =
        match lst with
        | [] -> []
        | hd :: tl -> if p mod 2 = 1 then f (p + 1) tl
                      else hd :: f (p + 1) tl
    in
    f 1 lst

let () =
    let lst = read_lines () in
    let res = remove_odd_position lst in
    List.iter (fun x -> print_int x; print_newline ()) res
