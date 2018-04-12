let rec make_array n = 
    if n = 0 then []
    else 1 :: make_array (n - 1)

let () =
    let n = read_int () in
    let lst = make_array n in
    List.iter (Printf.printf "%d ") lst
