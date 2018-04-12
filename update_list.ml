let rec read_lines () =
    try let now = read_int () in
        now :: read_lines ()
    with 
        End_of_file -> []

let rec update lst = 
    match lst with
    | [] -> []
    | hd :: tl -> if hd >= 0 then hd :: update tl
                  else (-hd) :: update tl

let () =
    let lst = read_lines () in
    let res = update lst in
    List.iter (fun x -> print_int x; print_newline ()) res
