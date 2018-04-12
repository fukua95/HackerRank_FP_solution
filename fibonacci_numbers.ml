let cal_fib n =
    if n = 1 then 0
    else 
        let rec f p fp fq = 
            if p = n then fp
            else f (p + 1) (fp + fq) fp
        in
        f 2 1 0

let () =
    let n = read_int () in
    let res = cal_fib n in
    let () = print_int res in
    print_newline ()
