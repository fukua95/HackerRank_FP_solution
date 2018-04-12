let solve (str : string) : (char list) * (int list) =
    let len = String.length str in
    let rec f cl il n p =
        if p = len - 1 then (str.[p] :: cl, n :: il)
        else if str.[p + 1] = str.[p] then f cl il (n + 1) (p + 1)
        else f (str.[p] :: cl) (n :: il) 1 (p + 1)
    in
    let (u,v) = f [] [] 1 0 in
    (List.rev u, List.rev v)

let rec print_list_pair (cl,il): unit =
    match cl,il with
    | h1 :: t1,h2 :: t2 -> let () = print_char h1 in 
                           let () = if h2 > 1 then print_int h2 in
                           print_list_pair (t1,t2)
    | _ -> print_newline ()

let () =
    let str = read_line () in
    print_list_pair (solve str)
