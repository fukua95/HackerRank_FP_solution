let solve (x : float) : float = 
    let rec f acc n u v = 
        if n = 10. then acc
        else (
            let new_u = x *. u in
            let new_v = n *. v in
            f (acc +. new_u /. new_v) (n +. 1.) new_u new_v
        )
    in
    f 1. 1. 1. 1.

let () =
    let n = read_int () in
    for i = 1 to n do 
        let now = read_float () in
        let res = solve now in
        let () = print_float res in
        print_newline ()
    done
