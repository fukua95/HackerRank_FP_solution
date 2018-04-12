let rec solve n = 
    if n > 0 then
        let () = print_string("Hello World\n") in
        solve (n - 1)

let () = 
    let n = read_int() in
    solve n
