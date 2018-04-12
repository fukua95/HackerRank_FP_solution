let mingling (n : int) (p : string) (q : string) : char list = 
    let rec f acc i =
        if i = n - 1 then (q.[i] :: p.[i] :: acc)
        else f (q.[i] :: p.[i] :: acc) (i + 1) 
    in
    List.rev (f [] 0)

let rec print_list lst = 
    match lst with
    | [] -> print_newline ()
    | hd :: tl -> (print_char hd; print_list tl)

let () =
    let p = read_line () in
    let q = read_line () in
    let n = String.length p in
    print_list (mingling n p q)
